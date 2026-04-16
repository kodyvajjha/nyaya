(* Head reductions used by the checker's whnf:
     β : applied lambda
     δ : constant unfolding
     ι : recursor applied to a constructor
     kernel Nat builtins
     string-literal expansion

   Each rule is pure relative to [Ctx.t] (only the declaration table
   and the current logger are observed). Rules that cascade into whnf
   receive it as an explicit callback, so the reducer stays free of
   the mutual-recursion machinery of the checker itself. *)

open Expr

(** [beta e] head-reduces any applied lambdas at the spine of [e]. *)
let beta e =
  let rec aux f args =
    match node f, args with
    | Lam { body; _ }, v :: vs ->
      aux (instantiate ~free_var:v ~expr:body) vs
    | _, _ -> mk_app f args
  in
  let f, args = get_apps e in
  aux f args

(** One-step δ-reduction of the head. Unfolds a [Const] into the value
    of its declaration, substituting universe parameters. *)
let delta_at_head (ctx : Ctx.t) f =
  match node f with
  | Const { name; uparams } ->
    let decl = Env.find (Ctx.env ctx) name in
    (match Decl.get_value decl with
    | Some v ->
      let decl_uparams = CCList.map Level.param (Decl.get_uparams decl) in
      Expr.subst_levels v decl_uparams uparams
    | None -> f)
  | _ -> f

(** Convert a string literal to its constructor representation:
      [StrLit "ok" → String.mk (List.cons Char (Char.ofNat 111)
                      (List.cons Char (Char.ofNat 107) (List.nil Char)))]
    Uses [Char.ofNat] for each Unicode code point, matching the Lean
    4 kernel's string_lit_to_constructor. *)
let string_lit_to_ctor (s : string) : Expr.t =
  let mk_name base field = Name.Str (Name.Str (Name.Anon, base), field) in
  let char_type = Expr.const (Name.Str (Name.Anon, "Char")) in
  let list_nil = Expr.mk_app (Expr.const (mk_name "List" "nil")) [ char_type ] in
  let list_cons_char =
    Expr.mk_app (Expr.const (mk_name "List" "cons")) [ char_type ]
  in
  let char_of_nat = Expr.const (mk_name "Char" "ofNat") in
  let string_mk = Expr.const (mk_name "String" "mk") in
  let code_points = ref [] in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let b0 = Char.code (String.get s !i) in
    let cp, advance =
      if b0 land 0x80 = 0 then
        b0, 1
      else if b0 land 0xe0 = 0xc0 then (
        let b1 = Char.code (String.get s (!i + 1)) in
        (b0 land 0x1f) lsl 6 lor (b1 land 0x3f), 2
      ) else if b0 land 0xf0 = 0xe0 then (
        let b1 = Char.code (String.get s (!i + 1)) in
        let b2 = Char.code (String.get s (!i + 2)) in
        ( (b0 land 0x0f) lsl 12
          lor ((b1 land 0x3f) lsl 6)
          lor (b2 land 0x3f),
          3 )
      ) else (
        let b1 = Char.code (String.get s (!i + 1)) in
        let b2 = Char.code (String.get s (!i + 2)) in
        let b3 = Char.code (String.get s (!i + 3)) in
        ( (b0 land 0x07) lsl 18
          lor ((b1 land 0x3f) lsl 12)
          lor ((b2 land 0x3f) lsl 6)
          lor (b3 land 0x3f),
          4 )
      )
    in
    code_points := cp :: !code_points;
    i := !i + advance
  done;
  let char_list =
    List.fold_left
      (fun acc cp ->
        let ch = Expr.mk_app char_of_nat [ Expr.natlit (Z.of_int cp) ] in
        Expr.mk_app list_cons_char [ ch; acc ])
      list_nil !code_points
  in
  Expr.mk_app string_mk [ char_list ]

(**
   One-step ι-reduction at the head of [e].

   Iota reduction fires when [e] is a recursor applied to a
   constructor-headed major premise.  The general shape of a reducible
   application is:

     R  p₁…pₙ  motive  minor₁…minorₖ  (C  q₁…qₘ  f₁…fⱼ)  s₁…sₜ

   where:
     - R          is the recursor constant
     - p₁…pₙ     are the recursor parameters
     - motive     is the motive (one or more)
     - minor₁…   are the minor premises (one per constructor)
     - C q… f…   is the major premise, already whnf'd to a
                 constructor application; q… are the constructor's
                 own type parameters and f… are its fields
                 (= the ctor_num_args trailing args)
     - s₁…sₜ     are any *extra* arguments that appear after the major
                 in the full spine (e.g. motive indices that trail the
                 major, or additional arguments to the overall type
                 such as the [List.below] proof in brecOn-based
                 recursors)

   The iota rule value [rule.value] is a closed term lambda-bound over
   (params, motives, minors, ctor_fields), i.e. it expects exactly
   [prefix @ field_args]. The extra suffix args [s₁…sₜ] are NOT part
   of the rule RHS; they must be passed on to the result.
*)
let iota_at_head (ctx : Ctx.t) (e : Expr.t) ~whnf : Expr.t =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  let hd, args = Expr.get_apps e in
  match Expr.node hd with
  | Expr.Const { name = rec_name; uparams = rec_levels } ->
    let decl =
      try Env.find (Ctx.env ctx) rec_name
      with Not_found -> raise Not_found
    in
    (match decl with
    | Decl.Rec { num_params; num_idx; num_motives; num_minors; rules; _ } ->
      let decl_uparams = CCList.map Level.param (Decl.get_uparams decl) in
      let major_idx = num_params + num_idx + num_motives + num_minors in
      if List.length args <= major_idx then
        e
      else (
        let major = List.nth args major_idx in
        let major_whnf = whnf ctx major in
        L.debug "iota_at_head: rec=%a major_idx=%d major_whnf=@[%a@]" Name.pp
          rec_name major_idx Expr.pp major_whnf;
        let maj_hd, maj_args = Expr.get_apps major_whnf in
        match node maj_hd with
        | Expr.Const { name = ctor_name; _ } ->
          let rule_opt =
            List.find_opt
              (fun (r : Decl.Rec_rule.t) -> r.ctor_name = ctor_name)
              rules
          in
          (match rule_opt with
          | None -> e
          | Some rule ->
            let prefix_len = num_params + num_motives + num_minors in
            let prefix = CCList.take prefix_len args in
            let suffix = CCList.drop (major_idx + 1) args in
            let maj_num_args = List.length maj_args in
            if maj_num_args < rule.ctor_num_args then
              e
            else (
              let field_args =
                maj_args
                |> List.rev
                |> CCList.take rule.ctor_num_args
                |> List.rev
              in
              let rule_val =
                Expr.subst_levels rule.value decl_uparams rec_levels
              in
              let new_args = prefix @ field_args @ suffix in
              let red = Expr.mk_app rule_val new_args in
              L.debug "iota: %a" Expr.pp red;
              red
            ))
        | Expr.Literal (Expr.NatLit n) ->
          (* NatLit iota: major is Nat literal. Convert to constructor
             form and apply the matching rule directly (not via another
             iota pass) to avoid looping with the Nat.succ kernel
             builtin. *)
          let mk_nat s = Name.Str (Name.Str (Name.Anon, "Nat"), s) in
          let ctor_name, field_args =
            if Z.equal n Z.zero then
              mk_nat "zero", []
            else
              mk_nat "succ", [ Expr.natlit (Z.pred n) ]
          in
          let rule_opt =
            List.find_opt
              (fun (r : Decl.Rec_rule.t) -> r.ctor_name = ctor_name)
              rules
          in
          (match rule_opt with
          | None -> e
          | Some rule ->
            let rule_val =
              Expr.subst_levels rule.value decl_uparams rec_levels
            in
            let prefix_len = num_params + num_motives + num_minors in
            let prefix = CCList.take prefix_len args in
            let suffix = CCList.drop (major_idx + 1) args in
            let new_args = prefix @ field_args @ suffix in
            let red = Expr.mk_app rule_val new_args in
            L.debug "iota (natlit): %a" Expr.pp red;
            red)
        | _ ->
          (* Structure eta: for single-constructor, index-free,
             non-recursive types, expand the major by projecting each
             field out. *)
          (match rec_name with
          | Name.Str (ind_name, _) ->
            (match Env.find_opt (Ctx.env ctx) ind_name with
            | Some (Decl.Inductive { ctor_names; num_idx; is_recursive; _ })
              when List.length ctor_names = 1
                   && num_idx = 0
                   && not is_recursive ->
              let rule = List.hd rules in
              let rule_val =
                Expr.subst_levels rule.value decl_uparams rec_levels
              in
              let prefix_len = num_params + num_motives + num_minors in
              let prefix = CCList.take prefix_len args in
              let suffix = CCList.drop (major_idx + 1) args in
              let field_args =
                List.init rule.ctor_num_args (fun i ->
                    Expr.proj ind_name i major)
              in
              let new_args = prefix @ field_args @ suffix in
              let red = Expr.mk_app rule_val new_args in
              L.debug "iota (struct-eta): %a" Expr.pp red;
              red
            | _ -> e)
          | _ -> e))
    | _ -> e)
  | _ -> e

(** Kernel builtin reductions for Nat literals.

    When the head constant is a known Nat builtin and the relevant
    arguments (after whnf) are NatLit values, compute the result
    directly. Returns [Some result] if a reduction fired, [None]
    otherwise.

    Reference: "Type Checking in Lean 4", §3.5 (Literals). *)
let nat_lit_reduce (ctx : Ctx.t) (e : Expr.t) ~whnf : Expr.t option =
  let hd, args = Expr.get_apps e in
  match Expr.node hd with
  | Expr.Const { name; _ } ->
    let mk_name s1 s2 = Name.Str (Name.Str (Name.Anon, s1), s2) in
    let as_nat_lit e =
      match Expr.node (whnf ctx e) with
      | Expr.Literal (Expr.NatLit n) -> Some n
      | _ -> None
    in
    let bool_const b =
      Expr.const (mk_name "Bool" (if b then "true" else "false"))
    in
    let unary f =
      match args with
      | [ a ] ->
        (match as_nat_lit a with
        | Some n -> Some (f n)
        | None -> None)
      | _ -> None
    in
    let binary f =
      match args with
      | [ a; b ] ->
        (match as_nat_lit a, as_nat_lit b with
        | Some m, Some n -> Some (f m n)
        | _ -> None)
      | _ -> None
    in
    (match name with
    | n when n = mk_name "Nat" "succ" ->
      unary (fun n -> Expr.natlit (Z.succ n))
    | n when n = mk_name "Nat" "add" ->
      (match binary (fun m n -> Expr.natlit (Z.add m n)) with
      | Some _ as r -> r
      | None ->
        (match args with
        | [ a; b ] ->
          (match as_nat_lit b with
          | Some n when Z.leq n (Z.of_int 64) ->
            if Z.equal n Z.zero then
              Some a
            else (
              let inner = Expr.mk_app hd [ a; Expr.natlit (Z.pred n) ] in
              Some (Expr.mk_app (Expr.const (mk_name "Nat" "succ")) [ inner ])
            )
          | _ ->
            let b' = whnf ctx b in
            let bhd, bargs = Expr.get_apps b' in
            (match Expr.node bhd, bargs with
            | Expr.Const { name = sn; _ }, [ y ] when sn = mk_name "Nat" "succ"
              ->
              let inner = Expr.mk_app hd [ a; y ] in
              Some (Expr.mk_app (Expr.const (mk_name "Nat" "succ")) [ inner ])
            | _ -> None))
        | _ -> None))
    | n when n = mk_name "Nat" "sub" ->
      (match binary (fun m n -> Expr.natlit (Z.max (Z.sub m n) Z.zero)) with
      | Some _ as r -> r
      | None ->
        let is_zero e =
          match Expr.node (whnf ctx e) with
          | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
          | _ -> false
        in
        let as_succ e =
          let e' = whnf ctx e in
          let shd, sargs = Expr.get_apps e' in
          match Expr.node shd, sargs with
          | Expr.Const { name = sn; _ }, [ x ] when sn = mk_name "Nat" "succ" ->
            Some x
          | _ ->
            (match Expr.node e' with
            | Expr.Literal (Expr.NatLit k) when Z.gt k Z.zero ->
              Some (Expr.natlit (Z.pred k))
            | _ -> None)
        in
        (match args with
        | [ a; b ] ->
          if is_zero b then
            Some (whnf ctx a)
          else if is_zero a then
            Some (Expr.natlit Z.zero)
          else (
            match as_succ a, as_succ b with
            | Some n, Some m -> Some (Expr.mk_app hd [ n; m ])
            | _ -> None
          )
        | _ -> None))
    | n when n = mk_name "Nat" "mul" ->
      (match binary (fun m n -> Expr.natlit (Z.mul m n)) with
      | Some _ as r -> r
      | None ->
        (match args with
        | [ a; b ] ->
          (match as_nat_lit b with
          | Some n when Z.leq n (Z.of_int 64) ->
            if Z.equal n Z.zero then
              Some (Expr.natlit Z.zero)
            else (
              let inner = Expr.mk_app hd [ a; Expr.natlit (Z.pred n) ] in
              Some (Expr.mk_app (Expr.const (mk_name "Nat" "add")) [ inner; a ])
            )
          | _ ->
            let b' = whnf ctx b in
            let bhd, bargs = Expr.get_apps b' in
            (match Expr.node bhd, bargs with
            | Expr.Const { name = sn; _ }, [ y ] when sn = mk_name "Nat" "succ"
              ->
              let inner = Expr.mk_app hd [ a; y ] in
              Some (Expr.mk_app (Expr.const (mk_name "Nat" "add")) [ inner; a ])
            | _ -> None))
        | _ -> None))
    | n when n = mk_name "Nat" "pow" ->
      binary (fun m n -> Expr.natlit (Z.pow m (Z.to_int n)))
    | n when n = mk_name "Nat" "div" ->
      (match
         binary (fun m n ->
             if Z.equal n Z.zero then
               Expr.natlit Z.zero
             else
               Expr.natlit (Z.div m n))
       with
      | Some _ as r -> r
      | None ->
        let is_zero e =
          match Expr.node (whnf ctx e) with
          | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
          | _ -> false
        in
        (match args with
        | [ a; _ ] when is_zero a -> Some (Expr.natlit Z.zero)
        | [ _; b ] when is_zero b -> Some (Expr.natlit Z.zero)
        | _ -> None))
    | n when n = mk_name "Nat" "mod" ->
      (match
         binary (fun m n ->
             if Z.equal n Z.zero then
               Expr.natlit Z.zero
             else
               Expr.natlit (Z.rem m n))
       with
      | Some _ as r -> r
      | None ->
        let is_zero e =
          match Expr.node (whnf ctx e) with
          | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
          | _ -> false
        in
        (match args with
        | [ a; _ ] when is_zero a -> Some (Expr.natlit Z.zero)
        | [ _; b ] when is_zero b -> Some (Expr.natlit Z.zero)
        | _ -> None))
    | n when n = mk_name "Nat" "beq" ->
      (match binary (fun m n -> bool_const (Z.equal m n)) with
      | Some _ as r -> r
      | None ->
        let is_zero e =
          match Expr.node (whnf ctx e) with
          | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
          | _ -> false
        in
        let as_succ e =
          let e' = whnf ctx e in
          let shd, sargs = Expr.get_apps e' in
          match Expr.node shd, sargs with
          | Expr.Const { name = sn; _ }, [ x ] when sn = mk_name "Nat" "succ" ->
            Some x
          | _ ->
            (match Expr.node e' with
            | Expr.Literal (Expr.NatLit k) when Z.gt k Z.zero ->
              Some (Expr.natlit (Z.pred k))
            | _ -> None)
        in
        (match args with
        | [ a; b ] ->
          (match as_succ a, as_succ b with
          | Some n, Some m -> Some (Expr.mk_app hd [ n; m ])
          | Some _, _ when is_zero b -> Some (bool_const false)
          | _, Some _ when is_zero a -> Some (bool_const false)
          | _ -> None)
        | _ -> None))
    | n when n = mk_name "Nat" "ble" ->
      (match binary (fun m n -> bool_const (Z.leq m n)) with
      | Some _ as r -> r
      | None ->
        let is_zero e =
          match Expr.node (whnf ctx e) with
          | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
          | _ -> false
        in
        let as_succ e =
          let shd, sargs = Expr.get_apps (whnf ctx e) in
          match Expr.node shd, sargs with
          | Expr.Const { name = sn; _ }, [ x ] when sn = mk_name "Nat" "succ" ->
            Some x
          | _ ->
            (match Expr.node (whnf ctx e) with
            | Expr.Literal (Expr.NatLit k) when Z.gt k Z.zero ->
              Some (Expr.natlit (Z.pred k))
            | _ -> None)
        in
        (match args with
        | [ a; b ] ->
          if is_zero a then
            Some (bool_const true)
          else (
            match as_succ a with
            | Some _ when is_zero b -> Some (bool_const false)
            | Some n ->
              (match as_succ b with
              | Some m -> Some (Expr.mk_app hd [ n; m ])
              | None -> None)
            | None -> None
          )
        | _ -> None))
    | n when n = mk_name "Nat" "land" ->
      binary (fun m n -> Expr.natlit (Z.logand m n))
    | n when n = mk_name "Nat" "lor" ->
      binary (fun m n -> Expr.natlit (Z.logor m n))
    | n when n = mk_name "Nat" "xor" ->
      binary (fun m n -> Expr.natlit (Z.logxor m n))
    | n when n = mk_name "Nat" "shiftLeft" ->
      binary (fun m n -> Expr.natlit (Z.shift_left m (Z.to_int n)))
    | n when n = mk_name "Nat" "shiftRight" ->
      binary (fun m n -> Expr.natlit (Z.shift_right m (Z.to_int n)))
    | n when n = mk_name "Nat" "testBit" ->
      binary (fun m n -> bool_const (Z.testbit m (Z.to_int n)))
    | _ -> None)
  | _ -> None

(** Returns [true] when [name] is a primitive Nat kernel builtin whose
    definition recurses linearly (O(n)) and must NOT be δ-unfolded
    when [nat_lit_reduce] fails on symbolic args.

    Bitwise operations (land, lor, xor, shiftLeft, shiftRight, testBit)
    are NOT guarded here: they are defined via [Nat.bitwise] which
    recurses logarithmically (divides by 2), making δ safe. They also
    need δ for their equation lemmas (e.g. Nat.lor.eq_1). *)
let is_nat_builtin_name (name : Name.t) : bool =
  let mk_name s = Name.Str (Name.Str (Name.Anon, "Nat"), s) in
  name = mk_name "succ"
  || name = mk_name "add"
  || name = mk_name "sub"
  || name = mk_name "mul"
  || name = mk_name "pow"
  || name = mk_name "div"
  || name = mk_name "mod"
  || name = mk_name "beq"
  || name = mk_name "ble"

let is_nat_builtin (e : Expr.t) : bool =
  let hd, _args = Expr.get_apps e in
  match Expr.node hd with
  | Expr.Const { name; _ } -> is_nat_builtin_name name
  | _ -> false
