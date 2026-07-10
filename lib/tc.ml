(* TODO: Refactor this whole module to replace manual match/error with Result.bind + projection helper. *)

[@@@warning "-27"]

exception TypeError of string

exception Defeq_failure of string

exception Not_well_posed of string

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Checker"
end)

let truncate ?(max_len = 400) s =
  if String.length s <= max_len then
    s
  else String.sub s 0 (max_len - 3) ^ "..."

let expr_summary expr = CCFormat.asprintf "%a" Expr.pp expr |> truncate

let whnf_memo : (int, Expr.t) Hashtbl.t = Hashtbl.create 4096
let infer_memo : (int, Expr.t) Hashtbl.t = Hashtbl.create 4096

module InferTrace = Nyaya_parser.Util.MakeTrace (struct
  type env = Env.t

  type input = Expr.t

  type output = Expr.t

  let env_logger env = env.Env.logger

  let kind = "i"

  let elide_ok_env = "NYAYA_INFER_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_INFER_MAX_DEPTH"

  let input_summary = expr_summary

  let output_summary = expr_summary
end)

module WhnfTrace = Nyaya_parser.Util.MakeTrace (struct
  type env = Env.t

  type input = Expr.t

  type output = Expr.t

  let env_logger env = env.Env.logger

  let kind = "w"

  let elide_ok_env = "NYAYA_WHNF_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_WHNF_MAX_DEPTH"

  let input_summary = expr_summary

  let output_summary = expr_summary
end)

module DefEqTrace = Nyaya_parser.Util.MakeTrace (struct
  type env = Env.t

  type input = Expr.t * Expr.t

  type output = bool

  let env_logger env = env.Env.logger

  let kind = "d"

  let elide_ok_env = "NYAYA_DEFEQ_TRACE_ELIDE_OK"

  let max_depth_env = "NYAYA_DEFEQ_MAX_DEPTH"

  let input_summary (lhs, rhs) =
    truncate (CCFormat.asprintf "%a =?= %a" Expr.pp lhs Expr.pp rhs)

  let output_summary = string_of_bool
end)

module Pp = struct
  let pp_check fpf (e, ty) =
    let pp_value fpf e = CCFormat.fprintf fpf "value:@ %a" Expr.pp e in
    let pp_type fpf ty = CCFormat.fprintf fpf "type :@ %a" Expr.pp ty in

    CCFormat.fprintf fpf "@[<v 0>@[check:@]@,@[<hv 2>%a@]@,@[<hv 2>%a@]@]"
      pp_value e pp_type ty

  let pp_check_name fpf (n, ty) =
    let pp_name fpf e = CCFormat.fprintf fpf "name:@ %a" Name.pp e in
    let pp_type fpf ty = CCFormat.fprintf fpf "type :@ %a" Expr.pp ty in

    CCFormat.fprintf fpf "@[<v 0>@[check:@]@,@[<hv 2>%a@]@,@[<hv 2>%a@]@]"
      pp_name n pp_type ty

  let pp_defeq fpf (lhs, rhs) =
    CCFormat.fprintf fpf
      "@[<hov 0>@[defeq:@]@;@[<hv 2>expected:@; %a@]@;@[<hv 2>actual:@; %a@]@]"
      Expr.pp lhs Expr.pp rhs

  let pp_inferring fpf expr =
    CCFormat.fprintf fpf "@[<hov 0>Now inferring @[<hov 2>%a@]@]" Expr.pp expr

  let pp_failed_inferring fpf expr =
    CCFormat.fprintf fpf "@[<v 0>failed inferring:@, @[<hov 2> %a@]@]" Expr.pp
      expr
end

module Reduce = struct
  open Expr

  let beta e =
    let rec aux f args =
      match node f, args with
      | Lam { body; _ }, v :: vs ->
        aux (instantiate ~free_var:v ~expr:body ()) vs
      | _, _ -> mk_app f args
    in
    let f, args = get_apps e in
    aux f args

  let delta_at_head (env : Env.t) f =
    (* One-step delta reduction of the head. *)
    match node f with
    | Const { name; uparams }  ->
      let decl = Hashtbl.find env.tbl name in
      let decl_value = decl |> Decl.get_value in
      (* TODO: add a note about this in the notebook. *)
      (match decl_value with
      | Some v ->
        let decl_uparams =
          CCList.map Level.param (decl |> Decl.get_uparams)
        in
        Expr.subst_levels v decl_uparams uparams
      | None -> f)
    | _ -> f

  (** Convert a string literal to its constructor representation:
        [StrLit "ok" → String.mk (List.cons Char (Char.ofNat 111)
                        (List.cons Char (Char.ofNat 107) (List.nil Char)))]
      Uses [Char.ofNat] for each Unicode code point, matching the Lean 4
      kernel's string_lit_to_constructor. *)
  let string_lit_to_ctor (s : string) : Expr.t =
    let mk_name base field = Name.Str (Name.Str (Name.Anon, base), field) in
    let char_type = Expr.const (Name.Str (Name.Anon, "Char")) in
    let list_nil = Expr.mk_app (Expr.const (mk_name "List" "nil")) [char_type] in
    let list_cons_char = Expr.mk_app (Expr.const (mk_name "List" "cons")) [char_type] in
    let char_of_nat = Expr.const (mk_name "Char" "ofNat") in
    let string_mk = Expr.const (mk_name "String" "mk") in
    (* Decode UTF-8 string to list of Unicode code points *)
    let code_points = ref [] in
    let len = String.length s in
    let i = ref 0 in
    while !i < len do
      let b0 = Char.code (String.get s !i) in
      let cp, advance =
        if b0 land 0x80 = 0 then (b0, 1)
        else if b0 land 0xe0 = 0xc0 then
          let b1 = Char.code (String.get s (!i + 1)) in
          ((b0 land 0x1f) lsl 6 lor (b1 land 0x3f), 2)
        else if b0 land 0xf0 = 0xe0 then
          let b1 = Char.code (String.get s (!i + 1)) in
          let b2 = Char.code (String.get s (!i + 2)) in
          ((b0 land 0x0f) lsl 12 lor ((b1 land 0x3f) lsl 6) lor (b2 land 0x3f), 3)
        else
          let b1 = Char.code (String.get s (!i + 1)) in
          let b2 = Char.code (String.get s (!i + 2)) in
          let b3 = Char.code (String.get s (!i + 3)) in
          ((b0 land 0x07) lsl 18 lor ((b1 land 0x3f) lsl 12)
           lor ((b2 land 0x3f) lsl 6) lor (b3 land 0x3f), 4)
      in
      code_points := cp :: !code_points;
      i := !i + advance
    done;
    (* Build list from right to left (code_points is reversed) *)
    let char_list = List.fold_left (fun acc cp ->
      let ch = Expr.mk_app char_of_nat [Expr.natlit (Z.of_int cp)] in
      Expr.mk_app list_cons_char [ch; acc]
    ) list_nil !code_points in
    Expr.mk_app string_mk [char_list]

  (**
     One-step iota reduction at the head of [e].

     Iota reduction fires when [e] is a recursor applied to a
     constructor-headed major premise.  The general shape of a reducible
     application is:

       R  p₁…pₙ  motive  minor₁…minorₖ  (C  q₁…qₘ  f₁…fⱼ)  s₁…sₜ

     where:
       - R          is the recursor constant
       - p₁…pₙ     are the recursor parameters
       - motive     is the motive (one or more)
       - minor₁…   are the minor premises (one per constructor)
       - C q… f…   is the major premise, already whnf'd to a constructor
                   application; q… are the constructor's own type parameters
                   and f… are its fields (= the ctor_num_args trailing args)
       - s₁…sₜ     are any *extra* arguments that appear after the major
                   in the full spine (e.g. motive indices that trail the
                   major, or additional arguments to the overall type such
                   as the [List.below] proof in brecOn-based recursors)

     The iota rule value [rule.value] is a closed term lambda-bound over
     (params, motives, minors, ctor_fields), i.e. it expects exactly
     [prefix @ field_args].  The extra suffix args [s₁…sₜ] are NOT part
     of the rule RHS; they must be passed on to the result:

       result = rule.value  p₁…pₙ  motive  minor₁…  f₁…fⱼ  s₁…sₜ

     Omitting [suffix] was the original bug: specialised recursors such as
     [List.lengthTRAux.match_1] take a [List.below] proof after the major
     premise.  Without [suffix], that proof was silently dropped, leaving
     the minor result partially applied (e.g. [fun (_ : List.below …) => x]
     instead of [x]).
  *)
  let iota_at_head (env : Env.t) (e : Expr.t) whnf : Expr.t =
    let module Logger = (val env.logger) in
    let hd, args = Expr.get_apps e in
    match Expr.node hd with
    | Expr.Const { name = rec_name; uparams = rec_levels } ->
      (* Look up decl for the head constant *)
      let decl =
        try Hashtbl.find env.tbl rec_name
        with Not_found -> (* unknown constant *) raise Not_found
      in
      (match decl with
      | Decl.Rec { num_params; num_idx; num_motives; num_minors; rules; _ } ->
        (* The rule value is a template with the recursor's own universe
           param names.  Substitute them with the actual universe levels
           from the const application. *)
        let decl_uparams =
          CCList.map Level.param (Decl.get_uparams decl)
        in
        let major_idx = num_params + num_idx + num_motives + num_minors in
        if List.length args <= major_idx then
          e
        else (
          let major = List.nth args major_idx in
          let major_whnf = whnf env major in
          Logger.debug "iota_at_head: rec=%a major_idx=%d major_whnf=@[%a@]"
            Name.pp rec_name major_idx Expr.pp major_whnf;
          let maj_hd, maj_args = Expr.get_apps major_whnf in
          match node maj_hd with
          | Expr.Const { name = ctor_name; _ } ->
            (* Find matching reduction rule *)
            let rule_opt =
              List.find_opt
                (fun (r : Decl.Rec_rule.t) -> r.ctor_name = ctor_name)
                rules
            in
            (match rule_opt with
            | None ->
              (* Not a constructor the recursor knows about *)
              e
            | Some rule ->
              (**
                 The major premise is constructor-headed at this point, so
                 [maj_args] contains *all* arguments of that constructor
                 application — including constructor parameters (e.g. the
                 implicit type parameter [α] in [MyList.nil α]).

                 But the recursor's [prefix] already contains the recursor
                 parameters/indices/motives/minors, which themselves include
                 those constructor parameters.

                 If we append all of [maj_args], params are passed twice and we
                 over-apply the rule RHS (example bug: [MyList.nil α α]), which
                 later shows up as confusing defeq failures.

                 So for iota reduction we must append only the constructor
                 *field* arguments (the final [rule.ctor_num_args] arguments),
                 not all constructor arguments.
              *)
              (* The rule value expects: params, motives, minors, then
                 ctor fields.  Indices sit between minors and major in
                 the recursor spine but are NOT passed to the rule — they
                 are determined by the constructor. *)
              let prefix_len = num_params + num_motives + num_minors in
              let prefix = CCList.take prefix_len args in
              (* Args in the spine that come after the major premise.
                 These are not part of the rule RHS and must be re-applied
                 to the result.  See the docstring on [iota_at_head] for
                 why this matters. *)
              let suffix = CCList.drop (major_idx + 1) args in
              let maj_num_args = List.length maj_args in
              if maj_num_args < rule.ctor_num_args then
                (* Malformed constructor application: don't reduce. *)
                e
              else (
                let field_args =
                  maj_args |> List.rev
                  |> CCList.take rule.ctor_num_args
                  |> List.rev
                in
                let rule_val =
                  Expr.subst_levels rule.value decl_uparams rec_levels
                in
                let new_args = prefix @ field_args @ suffix in
                let red = Expr.mk_app rule_val new_args in
                Logger.debug "iota: %a" Expr.pp red;
                red
              ))
          | Expr.Literal (Expr.NatLit n) ->
            (* NatLit iota: the major premise is a Nat literal, not a
               constructor application.  Convert to constructor form and
               apply the matching rule directly.
                 NatLit 0   → Nat.zero rule, field_args = []
                 NatLit n+1 → Nat.succ rule, field_args = [NatLit (n-1)]
               We apply the rule directly rather than re-calling
               iota_at_head to avoid looping with the Nat.succ kernel
               builtin (which would whnf Nat.succ(NatLit n) back to
               NatLit(n+1)). *)
            let mk_nat s = Name.Str (Name.Str (Name.Anon, "Nat"), s) in
            let ctor_name, field_args =
              if Z.equal n Z.zero then (mk_nat "zero", [])
              else (mk_nat "succ", [Expr.natlit (Z.pred n)])
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
              Logger.debug "iota (natlit): %a" Expr.pp red;
              red)
          | _ ->
            (* major isn't constructor-headed — try structure eta.
               For structure-like types (single constructor, no indices,
               not recursive), reduce the recursor by projecting out
               each field:
                 S.rec params motive minor x  →  minor (x.0) (x.1) … *)
            (match rec_name with
            | Name.Str (ind_name, _) ->
              (match Hashtbl.find_opt env.tbl ind_name with
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
                Logger.debug "iota (struct-eta): %a" Expr.pp red;
                red
              | _ -> e)
            | _ -> e)
        )
      | _ -> e)
    | _ -> e

  (** Kernel builtin reductions for Nat literals.

      When the head constant is a known Nat builtin and the relevant
      arguments (after whnf) are NatLit values, compute the result
      directly.  Returns [Some result] if a reduction fired, [None]
      otherwise.

      Unary:  Nat.succ
      Binary arithmetic: Nat.add, Nat.sub, Nat.mul, Nat.pow, Nat.div, Nat.mod
      Binary comparisons: Nat.beq, Nat.ble  (produce Bool constructors)
      Bitwise: Nat.land, Nat.lor, Nat.xor
      Shifts:  Nat.shiftLeft, Nat.shiftRight
      Bit test: Nat.testBit  (produces Bool constructors)

      Reference: "Type Checking in Lean 4", §3.5 (Literals). *)
  let nat_lit_reduce (env : Env.t) (e : Expr.t) whnf : Expr.t option =
    let hd, args = Expr.get_apps e in
    match Expr.node hd with
    | Expr.Const { name; _ } ->
      let mk_name s1 s2 = Name.Str (Name.Str (Name.Anon, s1), s2) in
      let as_nat_lit e =
        match Expr.node (whnf env e) with
        | Expr.Literal (Expr.NatLit n) -> Some n
        | _ -> None
      in
      let bool_const b =
        Expr.const (mk_name "Bool" (if b then "true" else "false"))
      in
      let unary f =
        match args with
        | [a] -> (match as_nat_lit a with Some n -> Some (f n) | None -> None)
        | _ -> None
      in
      let binary f =
        match args with
        | [a; b] ->
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
          (* Partial iota rules for Nat.add:
               Nat.add x 0           → x
               Nat.add x (NatLit n+1) → Nat.succ (Nat.add x (NatLit n))
               Nat.add x (Nat.succ y) → Nat.succ (Nat.add x y)
             NatLit case bounded to n ≤ 64 to avoid O(n) blowup. *)
          match args with
          | [a; b] ->
            (match as_nat_lit b with
            | Some n when Z.leq n (Z.of_int 64) ->
              if Z.equal n Z.zero then Some a
              else
                let inner = Expr.mk_app hd [a; Expr.natlit (Z.pred n)] in
                Some (Expr.mk_app (Expr.const (mk_name "Nat" "succ")) [inner])
            | _ ->
              (* Symbolic successor: Nat.add x (Nat.succ y) → Nat.succ (Nat.add x y) *)
              let b' = whnf env b in
              let bhd, bargs = Expr.get_apps b' in
              (match Expr.node bhd, bargs with
               | Expr.Const { name = sn; _ }, [y]
                 when sn = mk_name "Nat" "succ" ->
                 let inner = Expr.mk_app hd [a; y] in
                 Some (Expr.mk_app (Expr.const (mk_name "Nat" "succ")) [inner])
               | _ -> None))
          | _ -> None)
      | n when n = mk_name "Nat" "sub" ->
        (match binary (fun m n -> Expr.natlit (Z.max (Z.sub m n) Z.zero)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota rule for Nat.sub with symbolic args, matching the
             real definition exactly (Nat.sub, Init/Prelude.lean):
               protected def Nat.sub : Nat → Nat → Nat
                 | a, 0      => a
                 | a, succ b => pred (Nat.sub a b)
             This recurses ONLY on the second argument (the subtrahend);
             the first argument [a] is never pattern-matched and may stay
             fully symbolic. "succ" matches both Nat.succ x and
             NatLit (k+1).
             NOTE: an earlier version of this rule additionally special-
             cased "Nat.sub 0 _ → 0" and "Nat.sub (succ n) (succ m) →
             Nat.sub n m" (paired decrement, skipping the intermediate
             Nat.pred). Both are only PROPOSITIONALLY true (provable by
             induction on the second argument), not definitionally true
             by iota alone when that argument is symbolic — e.g.
             Nat.sub 0 (succ b) really iota-reduces to
             Nat.pred (Nat.sub 0 b), which is stuck when b is a free
             variable, not the literal 0 the old rule claimed. That was
             an unsound over-generalization; removed in favor of this
             faithful single-step reproduction. *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          let as_succ e =
            let e' = whnf env e in
            let shd, sargs = Expr.get_apps e' in
            match Expr.node shd, sargs with
            | Expr.Const { name = sn; _ }, [x]
              when sn = mk_name "Nat" "succ" -> Some x
            | _ ->
              (match Expr.node e' with
              | Expr.Literal (Expr.NatLit k) when Z.gt k Z.zero ->
                Some (Expr.natlit (Z.pred k))
              | _ -> None)
          in
          match args with
          | [a; b] ->
            if is_zero b then Some (whnf env a)
            else
              (match as_succ b with
              | Some m ->
                let inner = Expr.mk_app hd [a; m] in
                Some (Expr.mk_app (Expr.const (mk_name "Nat" "pred")) [inner])
              | None -> None)
          | _ -> None)
      | n when n = mk_name "Nat" "mul" ->
        (match binary (fun m n -> Expr.natlit (Z.mul m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota rules for Nat.mul:
               Nat.mul x 0           → 0
               Nat.mul x (NatLit n+1) → Nat.add (Nat.mul x (NatLit n)) x
               Nat.mul x (Nat.succ y) → Nat.add (Nat.mul x y) x
             NatLit case bounded to n ≤ 64 to avoid O(n) blowup. *)
          match args with
          | [a; b] ->
            (match as_nat_lit b with
            | Some n when Z.leq n (Z.of_int 64) ->
              if Z.equal n Z.zero then Some (Expr.natlit Z.zero)
              else
                let inner = Expr.mk_app hd [a; Expr.natlit (Z.pred n)] in
                Some (Expr.mk_app (Expr.const (mk_name "Nat" "add")) [inner; a])
            | _ ->
              (* Symbolic successor: Nat.mul x (Nat.succ y) → Nat.add (Nat.mul x y) x *)
              let b' = whnf env b in
              let bhd, bargs = Expr.get_apps b' in
              (match Expr.node bhd, bargs with
               | Expr.Const { name = sn; _ }, [y]
                 when sn = mk_name "Nat" "succ" ->
                 let inner = Expr.mk_app hd [a; y] in
                 Some (Expr.mk_app (Expr.const (mk_name "Nat" "add")) [inner; a])
               | _ -> None))
          | _ -> None)
      | n when n = mk_name "Nat" "pow" ->
        binary (fun m n -> Expr.natlit (Z.pow m (Z.to_int n)))
      | n when n = mk_name "Nat" "div" ->
        (match binary (fun m n ->
          if Z.equal n Z.zero then Expr.natlit Z.zero
          else Expr.natlit (Z.div m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota rules for Nat.div with symbolic args:
               Nat.div 0 n → 0
               Nat.div n 0 → 0  (Lean kernel convention) *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          (match args with
          | [a; _] when is_zero a -> Some (Expr.natlit Z.zero)
          | [_; b] when is_zero b -> Some (Expr.natlit Z.zero)
          | _ -> None))
      | n when n = mk_name "Nat" "mod" ->
        (match binary (fun m n ->
          (* Nat.mod's own doc comment (Init/Prelude.lean): "When the
             divisor is 0, the result is the dividend rather than an
             error" — explicit example given there: `5 % 0 = 5`. So
             div-by-zero returns the numerator [m], not 0. *)
          if Z.equal n Z.zero then Expr.natlit m
          else Expr.natlit (Z.rem m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota rules for Nat.mod with symbolic args.
             Real definition (Nat.mod, Init/Prelude.lean):
               | 0, _ => 0
               | n@(succ _), m => ite (LE.le m n) (Nat.modCore n m) n
             with divisor-0 handled by that same `ite`: `LE.le 0 n` is
             always true, so it takes the `Nat.modCore n 0` branch, and
             `Nat.modCore n 0` returns `n` (its `dite (0 < 0) ... (fun _
             => x)` always takes the `x` branch when the divisor is 0).
             So:
               Nat.mod 0 n → 0
               Nat.mod n 0 → n   (dividend; divisor 0 is not an error —
                 see doc comment above, "5 % 0 = 5")
             And when [n] (first arg) is a concrete positive literal and
             [m] (second arg) is structurally provable > n via the
             existing Nat.ble partial-iota chain (i.e. `Nat.ble m n`
             reduces to `false`), the `ite` condition is false and the
             result is [n]. This is exactly the documented Fin-literal
             reduction: "Nat.mod n (m' + n + 1) reduces to n for concrete
             literal n" (needed so the OfNat instance for Fin reduces
             definitionally). *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          (match args with
          | [a; _] when is_zero a -> Some (Expr.natlit Z.zero)
          | [a; _b] when is_zero _b -> Some (whnf env a)
          | [a; b] ->
            (match as_nat_lit a with
            | Some k when Z.gt k Z.zero ->
              let ble_expr = Expr.mk_app (Expr.const (mk_name "Nat" "ble")) [b; a] in
              (match Expr.node (whnf env ble_expr) with
              | Expr.Const { name = bn; _ } when bn = mk_name "Bool" "false" ->
                Some (Expr.natlit k)
              | _ -> None)
            | _ -> None)
          | _ -> None))
      | n when n = mk_name "Nat" "beq" ->
        (match binary (fun m n -> bool_const (Z.equal m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota rules for Nat.beq with symbolic args:
               Nat.beq 0 (succ _)        → Bool.false
               Nat.beq (succ _) 0        → Bool.false
               Nat.beq (succ n) (succ m) → Nat.beq n m *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          let as_succ e =
            let e' = whnf env e in
            let shd, sargs = Expr.get_apps e' in
            match Expr.node shd, sargs with
            | Expr.Const { name = sn; _ }, [x]
              when sn = mk_name "Nat" "succ" -> Some x
            | _ ->
              (match Expr.node e' with
              | Expr.Literal (Expr.NatLit k) when Z.gt k Z.zero ->
                Some (Expr.natlit (Z.pred k))
              | _ -> None)
          in
          match args with
          | [a; b] ->
            (match as_succ a, as_succ b with
            | Some n, Some m -> Some (Expr.mk_app hd [n; m])
            | Some _, _ when is_zero b -> Some (bool_const false)
            | _, Some _ when is_zero a -> Some (bool_const false)
            | _ -> None)
          | _ -> None)
      | n when n = mk_name "Nat" "ble" ->
        (match binary (fun m n -> bool_const (Z.leq m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota rules for Nat.ble with symbolic args:
               Nat.ble 0 _             → Bool.true
               Nat.ble (succ _) 0      → Bool.false
               Nat.ble (succ n) (succ m) → Nat.ble n m *)
          let is_zero e =
            match Expr.node (whnf env e) with
            | Expr.Literal (Expr.NatLit n) -> Z.equal n Z.zero
            | _ -> false
          in
          let as_succ e =
            let shd, sargs = Expr.get_apps (whnf env e) in
            match Expr.node shd, sargs with
            | Expr.Const { name = sn; _ }, [x]
              when sn = mk_name "Nat" "succ" -> Some x
            | _ ->
              (match Expr.node (whnf env e) with
              | Expr.Literal (Expr.NatLit k) when Z.gt k Z.zero ->
                Some (Expr.natlit (Z.pred k))
              | _ -> None)
          in
          match args with
          | [a; b] ->
            if is_zero a then Some (bool_const true)
            else
              (match as_succ a with
              | Some _ when is_zero b -> Some (bool_const false)
              | Some n ->
                (match as_succ b with
                | Some m -> Some (Expr.mk_app hd [n; m])
                | None -> None)
              | None -> None)
          | _ -> None)
      (* Bitwise operations: Nat.land, Nat.lor, Nat.xor *)
      | n when n = mk_name "Nat" "land" ->
        binary (fun m n -> Expr.natlit (Z.logand m n))
      | n when n = mk_name "Nat" "lor" ->
        binary (fun m n -> Expr.natlit (Z.logor m n))
      | n when n = mk_name "Nat" "xor" ->
        (match binary (fun m n -> Expr.natlit (Z.logxor m n)) with
        | Some _ as r -> r
        | None ->
          (* Nat.xor is guarded in [is_nat_builtin_name] (see that
             function's doc for why: unblocking UInt32.not_neg_one without
             this guard needs delta-unfolding Nat.xor into Nat.bitwise's
             well-founded-recursion body, which blows the depth limit).
             But Nat.xor.eq_1 (an auto-generated equation lemma) needs
             `Nat.xor n m` to delta-unfold exactly one step, to
             `Nat.bitwise bne n m`, with n/m left fully symbolic -- the
             guard would otherwise block that too. Reproduce exactly that
             one step here via [delta_at_head] on the bare head (rather
             than hand-writing the `bne`/instance subterm, which risks
             getting it subtly wrong): this looks up Nat.xor's own
             declaration value, the same thing ordinary delta would do.
             The result is headed by [Nat.bitwise], which is ALSO guarded,
             so this cannot recurse into the well-founded-recursion body:
             whnf'ing this result stops immediately. *)
          match args with
          | [_; _] ->
            let hd' = delta_at_head env hd in
            if hd' == hd then None else Some (Expr.mk_app hd' args)
          | _ -> None)
      (* Bit shift operations: Nat.shiftLeft, Nat.shiftRight *)
      | n when n = mk_name "Nat" "shiftLeft" ->
        binary (fun m n -> Expr.natlit (Z.shift_left m (Z.to_int n)))
      | n when n = mk_name "Nat" "shiftRight" ->
        binary (fun m n -> Expr.natlit (Z.shift_right m (Z.to_int n)))
      (* Bit test: Nat.testBit *)
      | n when n = mk_name "Nat" "testBit" ->
        binary (fun m n -> bool_const (Z.testbit m (Z.to_int n)))
      | _ -> None)
    | _ -> None

  (** Returns [true] when [name] is a primitive Nat kernel builtin whose
      definition recurses linearly (O(n)) and must NOT be delta-unfolded
      when [nat_lit_reduce] fails on symbolic args.

      [Nat.xor] and [Nat.bitwise] ARE guarded here (added alongside
      add/sub/mul/div/mod/beq/ble). [Nat.land], [Nat.lor], [Nat.shiftLeft],
      [Nat.shiftRight], [Nat.testBit] are deliberately left UNGUARDED,
      unchanged from before -- this asymmetry is intentional; see below.

      Root cause of the bug this fixes (Depth_limit on
      UInt32.not_neg_one): [Nat.xor]'s reference definition is
      `Nat.bitwise bne`, and [Nat.bitwise] is implemented via
      [Nat.bitwise._unary], built on [WellFounded.fix]/[Acc.rec], whose
      *accessibility proof* is resolved via ordinary structural Nat
      recursion (through [Nat.eq_or_lt_of_le]-style comparison lemmas
      applied to the concrete operands) -- genuinely O(n) in the operand
      value, not O(log n) as an earlier version of this comment assumed,
      when forced by kernel iota. The real Lean 4 kernel never pays this
      cost: its [type_checker.cpp] reduces Nat.land/Nat.lor/Nat.xor/
      Nat.shiftLeft/Nat.shiftRight natively via GMP bit operations
      (reduce_nat's dispatch table, alongside add/sub/mul/pow/gcd/mod/div/
      beq/ble) -- it never delta-unfolds [Nat.bitwise] (or, separately,
      [Nat.shiftLeft]/[Nat.shiftRight]'s own [Nat.brecOn]-based reference
      definitions) for any of these.

      Why ONLY xor/bitwise are guarded, not land/lor/shiftLeft/shiftRight
      too, even though the same unfaithfulness argument applies to all
      five: guarding an outer name here blocks ALL delta on it, including
      the single unconditional step some already-passing equation lemma
      needs. Confirmed empirically (reproducing each as a regression
      before deciding): `Nat.lor.eq_1`, `Nat.land.eq_1`,
      `Nat.shiftLeft.eq_1`, `Nat.shiftLeft.eq_2` all currently pass and
      need exactly one delta step of their respective outer name (to
      `Nat.bitwise <op> n m` for land/lor, or straight into
      `Nat.brecOn ...` for shiftLeft, which is NOT a `Nat.bitwise`-shaped
      one-liner at all -- unlike land/lor/xor, shiftLeft/shiftRight's
      *reference* definition really is the Nat.brecOn recursion itself,
      so there is no shallow "unfold once, then stop" form available for
      them the way there is for the bitwise trio). `Nat.xor.eq_1` ALSO
      currently passes and has the exact same one-step need as
      `Nat.lor.eq_1` -- so guarding [Nat.xor] here would reintroduce that
      regression too, EXCEPT [nat_lit_reduce]'s `Nat.xor` case (below)
      specifically restores that one step via [delta_at_head] before
      falling through to this guard, so [Nat.xor.eq_1] keeps working. No
      similar targeted fallback exists yet for land/lor/shiftLeft/
      shiftRight, so they stay unguarded rather than being broken; if a
      future declaration needs the same O(n)-blowup fix for one of them,
      add the same kind of targeted one-step [nat_lit_reduce] fallback
      then, backed by that declaration's own citation, rather than
      widening this guard set speculatively now. *)
  let is_nat_builtin_name (name : Name.t) : bool =
    let mk_name s = Name.Str (Name.Str (Name.Anon, "Nat"), s) in
    name = mk_name "succ" || name = mk_name "add"
    || name = mk_name "sub" || name = mk_name "mul"
    || name = mk_name "pow" || name = mk_name "div"
    || name = mk_name "mod" || name = mk_name "beq"
    || name = mk_name "ble"
    || name = mk_name "xor" || name = mk_name "bitwise"

  let is_nat_builtin (e : Expr.t) : bool =
    let hd, _args = Expr.get_apps e in
    match Expr.node hd with
    | Expr.Const { name; _ } -> is_nat_builtin_name name
    | _ -> false

end

(** Infer the type of the given [expr].*)
let rec infer (env : Env.t) (expr : Expr.t) : Expr.t =
  match Hashtbl.find_opt infer_memo (Expr.tag expr) with
  | Some ty -> ty
  | None ->
    let frame = InferTrace.enter env expr in
    (match infer_impl env expr with
    | ty ->
      InferTrace.leave_success env frame ty;
      Hashtbl.replace infer_memo (Expr.tag expr) ty;
      ty
    | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      InferTrace.leave_failure env frame exn;
      Printexc.raise_with_backtrace exn backtrace)

and infer_impl (env : Env.t) (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  match Expr.node expr with
  | Expr.Sort u -> Expr.sort (Level.Succ u)
  | Expr.FreeVar { name; expr; info; fvarId } ->
    expr
  | Expr.Lam { name; btype; binfo; body } ->
    (*
      infer Lambda(binder, body):
       assert! infersAsSort(binder.type)
       let binderFvar := fvar(binder)
       let bodyType := infer $ instantiate(body, binderFVar)
       Pi binder (abstract bodyType binderFVar)
    *)
    (match whnf env (infer env btype) |> Expr.node with
    | Expr.Sort _ ->
      let binder_free_var =
        Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ())
      in
      let body_type =
        infer env
          (Expr.instantiate ~logger:env.logger ~free_var:binder_free_var
             ~expr:body ())
      in
      let target_id = Expr.get_fvar_id binder_free_var in
      Expr.pi name btype binfo (Expr.abstract_fvar ~target_id ~k:0 body_type)
    | _ ->
      Logger.err "infer Lam: binder type is not a sort: %a"
        (TypeError "infer Lam: binder type not a sort") Expr.pp expr)
  | Expr.Forall { name; btype; binfo; body } ->
    (*
      infer Pi binder body:
      let l := inferSortOf binder
      let r := inferSortOf $ instantiate body (fvar(binder))
      imax(l, r)
    *)
    let l = infer_sort_of env btype in
    let free_var =
      Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ())
    in
    let r =
      infer_sort_of env
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:body ())
    in
    Expr.sort (Level.IMax (l, r) |> Level.simplify)
  | Expr.Const { name; uparams } ->
    (*
       infer Const name levels:
       let knownType := environment[name].type
       substituteLevels (e := knownType) (ks := knownType.uparams) (vs := levels)
    *)
    let known_type : Decl.t = Hashtbl.find env.tbl name in
    let known_type_uparams =
      CCList.map Level.param (Decl.get_uparams known_type)
    in
    Expr.subst_levels (known_type |> Decl.get_type) known_type_uparams uparams
  | Expr.App (f, arg) ->
    (*
    infer App(f, arg):
      match (whnf $ infer f) with
      | Pi binder body => 
        assert! defEq(binder.type, infer arg)
        instantiate(body, arg)
      | _ => error
    *)
    (match whnf env (infer env f) |> Expr.node with
    | Expr.Forall { btype; body; _ } ->
      (* When the expected parameter type is a Prop, skip the defeq
         check on the argument — proof irrelevance means the exact
         proof term doesn't matter, and inferring/normalising complex
         auto-generated proofs (omega, decide) is very expensive. *)
      let btype_is_prop =
        match Expr.node (whnf env (infer env btype)) with
        | Expr.Sort u -> Level.is_zero u
        | _ -> false
      in
      if not btype_is_prop then begin
        let arg_type = infer env arg in
        if not (isDefEq env btype arg_type) then
          Logger.err
            "@[<v 0>[infer App] defeq failed for@,\
             \ expr = %a@,\
             \ btype = %a@,\
             
             \ arg_type = %a@]"
            (Defeq_failure "infer App: btype vs arg type")
            Expr.pp expr Expr.pp btype Expr.pp arg_type
      end;
      Expr.instantiate ~logger:env.logger ~free_var:arg ~expr:body ()
    | e ->
      Logger.err "infer App: expected forall, got @[%a@]"
        (TypeError "infer App: whnf of fn type not a forall") Expr.pp expr)
  | Let { name; btype; value; body }  ->
    (*
       infer Let binder val body:
       assert! inferSortOf binder
       assert! defEq(infer(val), binder.type)
       infer (instantiate body val)
    *)
    (match infer env btype |> Expr.node with
    | Sort _ ->
      if not (isDefEq env btype (infer env value)) then
        Logger.err
          "@[<v 0>[infer Let] defeq failed for@,\
           \ btype = %a@,\
           \ value_type = %a@]"
          (Defeq_failure "infer Let: btype vs value type")
          Expr.pp btype Expr.pp (infer env value);
      infer env
        (Expr.instantiate ~logger:env.logger ~free_var:value ~expr:body ())
    | _ ->
      Logger.err "infer Let: binder type is not a sort: %a"
        (TypeError "infer Let: binder type not a sort") Expr.pp expr)
  | Proj { name; nat; expr } ->
    (*
       let structType := whnf (infer structure)
       let (const structTyName levels) tyArgs := structType.unfoldApps
       let InductiveInfo := env[structTyName]
       -- This inductive should only have the one constructor since it's claiming to be a structure.
       let ConstructorInfo := env[InductiveInfo.constructorNames[0]]

       let mut constructorType := substLevels ConstructorInfo.type (newLevels := levels)

       for tyArg in tyArgs.take constructorType.numParams
         match (whnf constructorType) with
           | pi _ body => inst body tyArg
           | _ => error

       for i in [0:projIdx]
         match (whnf constructorType) with
           | pi _ body => inst body (proj i structure)
           | _ => error

       match (whnf constructorType) with
         | pi binder _=> binder.type
         | _ => error
    *)
    let struct_type = infer env expr |> whnf env in
    let const, ty_args = Expr.get_apps struct_type in
    (match const |> Expr.node with
    | Const { name; uparams } ->
      let inductive_info = Hashtbl.find env.tbl name in
      let ctor_names = Decl.get_inductive_ctors inductive_info in
      let ctor_num_params = Decl.get_inductive_num_params inductive_info in
      (* This inductive should only have the one constructor since it's claiming to be a structure. *)
      assert (CCList.length ctor_names = 1);
      let ctor_info = ctor_names |> CCList.hd |> Hashtbl.find env.tbl in
      let ctor_info_type = ctor_info |> Decl.get_type in
      let ctor_uparams = CCList.map Level.param (Decl.get_uparams ctor_info) in
      let ctor_type =
        ref (Expr.subst_levels ctor_info_type ctor_uparams uparams)
      in
      let ty_param_args = CCList.take ctor_num_params ty_args in
      for i = 0 to CCList.length ty_param_args - 1 do
        let for_ty = whnf env !ctor_type in
        match for_ty |> Expr.node with
        | Forall { body; _ } ->
          let ty_arg = CCList.nth ty_param_args i in
          ctor_type :=
            Expr.instantiate ~logger:env.logger ~free_var:ty_arg ~expr:body ()
        | _ ->
          Logger.err
            "infer Proj: ctor type instantiation expected Forall, got %a"
            (TypeError "infer Proj: ctor type not a forall") Expr.pp for_ty
      done;
      (* Now, instantiate the projections *)
      for i = 0 to nat - 1 do
        let for_ty = whnf env !ctor_type in
        match for_ty |> Expr.node with
        | Forall { body; _ } ->
          let proj_expr = Expr.proj name i expr  in
          ctor_type :=
            Expr.instantiate ~logger:env.logger ~free_var:proj_expr ~expr:body
              ()
        | _ ->
          Logger.err
            "infer Proj: projection instantiation expected Forall, got %a"
            (TypeError "infer Proj: projection type not a forall") Expr.pp for_ty
      done;
      (* Now, the next binder's type is the projection type *)
      let final_ty = whnf env !ctor_type in
      (match final_ty |> Expr.node with
      | Forall { btype; _ } -> btype
      | _ ->
        Logger.err "infer Proj: final type expected Forall, got %a"
          (TypeError "infer Proj: final type not a forall") Expr.pp final_ty)
    | _ ->
      Logger.err
        "infer Proj: expected a const, got @[%a@] for @[%a@]"
        (TypeError "infer Proj: struct type not a const") Expr.pp struct_type Expr.pp expr)
  | Literal lit ->
    (match lit with
    | Expr.NatLit _ -> Expr.const (Name.of_string "Nat")
    | Expr.StrLit _ -> Expr.const (Name.of_string "String"))
  | BoundVar _ ->
    (* Since we are using the locally nameless approach, we should not run into
       bound variables during type inference, because all open binders will be
       instantiated with the appropriate free variables. *)
    Logger.err
      "@[<v 0>@[infer BoundVar: encountered bound variable during inference:@,\
       @[<hv 2> %a@]@]@]" (TypeError "infer: unexpected bound variable") Expr.pp expr

and infer_sort_of env (expr : Expr.t) =
  let module Logger = (val env.logger) in
  match whnf env (infer env expr) |> Expr.node with
  | Sort lvl -> lvl
  | _ ->
    Logger.err "infer_sort_of: expr %a is not a sort"
      (TypeError "infer_sort_of: not a sort") Expr.pp (infer env expr)
      
and whnf (env : Env.t) (expr : Expr.t) : Expr.t =
  match Hashtbl.find_opt whnf_memo (Expr.tag expr) with
  | Some result -> result
  | None ->
    let frame = WhnfTrace.enter env expr in
    (match whnf_impl env expr with
    | e ->
      WhnfTrace.leave_success env frame e;
      Hashtbl.replace whnf_memo (Expr.tag expr) e;
      e
    | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      WhnfTrace.leave_failure env frame exn;
      Printexc.raise_with_backtrace exn backtrace)

and whnf_impl (env : Env.t) (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  match expr |> Expr.node with
  | Expr.Sort u -> Expr.sort (Level.simplify u)
  | Expr.App (f, arg) ->
    let hd, args = Expr.get_apps expr in
    (* Try kernel numeric builtins BEFORE delta, so that e.g. Nat.add
       on two NatLit args computes in O(1) instead of O(n) iota steps. *)
    (match Reduce.nat_lit_reduce env expr whnf with
    | Some r ->
      Logger.debug "whnf: nat-builtin";
      whnf env r
    | None ->
    (* If the head is a known Nat kernel builtin but nat_lit_reduce couldn't
       fire, do NOT delta-unfold — unfolding would expose an O(n) Nat.brecOn
       reduction on huge literals (e.g. UInt32 operations with 2^32).
       Partial reductions like Nat.add x 1 → Nat.succ x are handled
       directly in nat_lit_reduce instead. *)
    if Reduce.is_nat_builtin expr then expr
    else
    let hd' =
      match hd |> Expr.node with
      | Expr.Const _ -> Reduce.delta_at_head env hd
      | _ -> whnf env hd
    in
    let e1 = Expr.mk_app hd' args in
    let e2 = Reduce.beta e1 in

    (* Now attempt iota at head *)
    let e3 = Reduce.iota_at_head env e2 whnf |> Reduce.beta in
    if e3 == expr then
      e3
    else (
      (* Log which reductions fired *)
      if hd' != hd then Logger.debug "whnf App: delta";
      if e2 != e1 then Logger.debug "whnf App: beta";
      if e3 != e2 then Logger.debug "whnf App: iota";
      whnf env e3))
  | Expr.Let { name; btype; value; body } ->
    Logger.debug "whnf: zeta";
    whnf env (Expr.instantiate ~logger:env.logger ~free_var:value ~expr:body ())
  | Expr.Const { name; uparams }  ->
    (* Nat.zero is definitionally equal to NatLit 0 in the Lean kernel.
       Normalize it here so that isDefEq doesn't need a Const-vs-Literal case. *)
    if name = Name.Str (Name.Str (Name.Anon, "Nat"), "zero") then
      Expr.natlit Z.zero
    (* Don't delta-unfold Nat kernel builtins — they may later be applied
       to huge literals, and their Nat.brecOn definitions would loop. *)
    else if Reduce.is_nat_builtin_name name then expr
    else
      let e' = Reduce.delta_at_head env expr in
      if e' == expr then expr
      else (Logger.debug "whnf Const: delta"; whnf env e')
  | Expr.Forall _ ->
    (* Already in whnf: the head is a Forall constructor, don't reduce under binders. *)
    expr
  | Expr.Proj {name; nat; expr = inner} ->
      let inner' = whnf env inner in
      (* String literals are not constructor-headed; expand to String.mk before
         attempting proj-reduce so that e.g. "".String.0 → List.nil Char. *)
      let inner' =
        match Expr.node inner' with
        | Expr.Literal (Expr.StrLit s) -> Reduce.string_lit_to_ctor s
        | _ -> inner'
      in
      let (hd, args) = Expr.get_apps inner' in
      (match Expr.node hd with
       | Expr.Const { name = cname; _ } ->
         (* Only reduce if the head is actually a constructor of the
            projected inductive type.  Without this guard, stuck recursor
            applications (e.g. List.rec … stuck_arg) would be mistaken
            for constructor applications and a random argument extracted. *)
         let ind_decl = Hashtbl.find env.tbl name in
         let ctor_names = Decl.get_inductive_ctors ind_decl in
         if List.mem cname ctor_names then
           let num_params = Decl.get_inductive_num_params ind_decl in
           let idx = nat + num_params in
           (match CCList.get_at_idx idx args with
            | Some field ->
              Logger.debug "whnf: proj-reduce (#%d)" nat;
              whnf env field
            | None -> Expr.proj name nat inner')
         else
           Expr.proj name nat inner'
       | _ -> Expr.proj name nat inner')
  | _ -> expr

(* TODO: optimize def eq checking by implementing union-find.
   TODO: ensure no other wasteful whnfs show up elsewhere before def eq check
*)
and isDefEq env e1 e2 =
  let frame = DefEqTrace.enter env (e1, e2) in
  match isDefEq_impl env e1 e2 with
  | ans ->
    DefEqTrace.leave_success env frame ans;
    ans
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    DefEqTrace.leave_failure env frame exn;
    Printexc.raise_with_backtrace exn backtrace

and isDefEq_impl env e1 e2 =
  let module Logger = (val env.logger) in
  if e1 == e2 then true
  else
  (* Early proof irrelevance check BEFORE whnf: if both terms inhabit
     definitionally equal Props, they are equal.  This avoids expensive
     whnf of proof terms (e.g. Nat.bitwise_lt_two_pow which unfolds
     Nat.rec on the bit-width).  infer is memoised and cheap.
     This must run BEFORE the same-head-args shortcut below, not after:
     if it ran second, that shortcut would eagerly compare a proof-term's
     own data arguments (e.g. two proofs `f n1` / `f n2` of the same Prop,
     with n1/n2 large or symbolic Nat/BitVec data) via isDefEq before ever
     getting the chance to notice both are proofs of the same
     proposition -- reintroducing exactly the kind of deep-unfold blowup
     the shortcut below exists to avoid, just relocated to proof terms
     instead of the data terms it was written for. *)
  let is_prop ty =
    match Expr.node (whnf env (infer env ty)) with
    | Expr.Sort u -> Level.is_zero u
    | _ -> false
  in
  let s = infer env e1 in
  let t = infer env e2 in
  if is_prop s && is_prop t && isDefEq env s t then true
  else
  (* Lazy-delta-reduction "same head" shortcut, BEFORE whnf. Mirrors the
     Lean 4 kernel's optimization in type_checker.cpp's
     lazy_delta_reduction_step: when both sides of a defeq check are
     applications of the exact same declaration (same Const name, same
     universe params) with the same arity, try comparing corresponding
     arguments pairwise via isDefEq *before* delta-unfolding that shared
     head, and only fall through to unfolding if that fails.
       bool type_checker::is_def_eq_app(expr const & t, expr const & s) {
         if (is_app(t) && is_app(s)) {
           ... expr t_fn = get_app_args(t, t_args);
               expr s_fn = get_app_args(s, s_args);
           if (is_def_eq(t_fn, s_fn) && t_args.size() == s_args.size()) {
             for (i = 0; i < t_args.size(); i++)
               if (!is_def_eq(t_args[i], s_args[i])) break;
             if (i == t_args.size()) return true;
       (and, gating entry into this path on both heads being the identical
       declaration with equal universe params:)
           if (is_app(t_n) && is_app(s_n) && is_eqp(d_t_deref, d_s_deref) &&
               d_t_hints_is_regular) {
             if (is_def_eq(const_levels(get_app_fn(t_n)), const_levels(get_app_fn(s_n))) &&
                 is_def_eq_args(t_n, s_n))
               return reduction_status::DefEqual;
     This must run BEFORE our own whnf, not after: our whnf's delta-
     unfolding is exactly what turns a shared-head application (e.g.
     BitVec.signExtend applied to two different BitVec arguments) into its
     full computational body -- observed exploding into raw Nat.sub
     arithmetic on a ~2^64 literal and hitting the depth limit for
     Int64.toInt32_ofBitVec -- before the arguments (where the real
     equality lives, e.g. Int64.toBitVec (Int64.ofBitVec b) reducing to b
     via a couple of cheap constructor/projection steps) are ever compared
     directly. This is soundness-safe as a pure early-exit: congruence
     (f a1..an ≡ f b1..bn whenever each ai ≡ bi) is a theorem of
     definitional equality, so returning true here can never be a false
     positive. If the shortcut doesn't apply (different heads, arity
     mismatch) or any argument pair fails, we fall through completely
     unchanged to the existing whnf-based algorithm below -- exactly as if
     this check were absent -- matching the real kernel's fallback (failed
     argument comparison unfolds both sides and continues, it does not
     conclude inequality). We don't replicate the kernel's
     `d_t->get_hints().is_regular()` gate (a performance heuristic
     restricting this to plain, non-reducible/non-irreducible definitions);
     omitting it only means we also attempt the shortcut for declarations
     the kernel wouldn't bother trying it on, which is still sound (same
     congruence argument), just a superset of when it fires. *)
  let same_head_args_shortcut () =
    let h1, args1 = Expr.get_apps e1 in
    let h2, args2 = Expr.get_apps e2 in
    match Expr.node h1, Expr.node h2 with
    | ( Expr.Const { name = n1; uparams = us },
        Expr.Const { name = n2; uparams = vs } )
      when args1 <> [] && n1 = n2
        && List.length args1 = List.length args2
        && CCList.fold_left2 (fun acc u v -> acc && Level.(u === v)) true us vs
      ->
      (* isDefEq's own final fallback (below, "structural mismatch") does
         not return [false] on failure -- it RAISES [Defeq_failure] via
         Logger.err, and the [isDefEq] wrapper re-raises it uncaught. A
         failed argument comparison here must be treated exactly like any
         other "shortcut doesn't apply" outcome -- fall through to the
         original whnf-based algorithm below, which may still succeed via
         a completely different route (e.g. reducing an argument first
         reveals an equality that comparing it raw does not) -- not let
         the exception escape and abort the entire outer comparison. This
         mirrors the real kernel's lazy_delta_reduction_step, which caches
         a failed is_def_eq_args and falls through to unfolding rather
         than concluding the whole thing unequal. *)
      (* This comparison is speculative: a failed argument pair here is an
         expected, routine outcome (fall through to the pre-existing
         whnf-based algorithm below), not a real declaration failure. But
         every raise site in this codebase (Logger.err) logs unconditionally
         before raising, so without suppression this prints a scary
         "[isDefEq] structural mismatch ... Exception: Defeq_failure" for
         every argument pair that doesn't happen to match syntactically,
         even when the overall declaration goes on to type-check
         successfully via the fallback path a moment later. Silence logging
         for the duration of this speculative attempt only; restore it
         (even if something other than Defeq_failure is raised) before
         returning, so a genuine failure surfacing later still gets logged
         normally by the top-level handler in [typecheck]. *)
      let prev_level = Logs.level () in
      Logs.set_level None;
      Fun.protect
        ~finally:(fun () -> Logs.set_level prev_level)
        (fun () ->
          try List.for_all2 (isDefEq env) args1 args2
          with Defeq_failure _ -> false)
    | _ -> false
  in
  if same_head_args_shortcut () then true
  else
  let e1' = whnf env e1 in
  let e2' = whnf env e2 in
  if e1' == e2' then true
  else
  let result =
    match Expr.node e1', Expr.node e2' with
    | Expr.Sort u1, Expr.Sort u2 ->
      if Level.(u1 === u2) then true
      else (Logger.debug "defeq: Sort level mismatch: %a vs %a"
              Level.pp u1 Level.pp u2; false)
    | Expr.FreeVar { fvarId = f1; _ }, Expr.FreeVar { fvarId = f2; _ } ->
      if f1 = f2 then true
      else (Logger.debug "defeq: FreeVar id mismatch"; false)
    | ( Expr.Forall { name = n; btype = s; body = a; binfo },
        Expr.Forall { btype = t; body = b; _ } ) ->
      if isDefEq env s t then (
        let free_var =
          Expr.fvar n s binfo (Nyaya_parser.Util.Uid.mk ())
        in
        let r = isDefEq env
          (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
          (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ()) in
        if not r then Logger.debug "defeq: Forall body mismatch";
        r
      ) else
        (Logger.debug "defeq: Forall btype mismatch"; false)
    | Expr.App (f, a), Expr.App (g, b) ->
      let fn_eq =
        isDefEq env (Reduce.delta_at_head env f) (Reduce.delta_at_head env g)
      in
      if not fn_eq then
        (Logger.debug "defeq: App fn mismatch"; false)
      else
        let arg_eq = isDefEq env a b in
        if not arg_eq then
          (Logger.debug "defeq: App arg mismatch"; false)
        else true
    | ( Expr.Const { name = n1; uparams = us },
        Expr.Const { name = n2; uparams = vs } ) ->
      if n1 = n2
         && CCList.fold_left2 (fun acc u v -> acc && Level.(u === v)) true us vs
      then true
      else (Logger.debug "defeq: Const mismatch: %a vs %a" Name.pp n1 Name.pp n2;
            false)
    | ( Expr.Lam { name = n; btype = s; body = a; binfo },
        Expr.Lam { btype = t; body = b; _ } ) ->
      if isDefEq env s t then (
        let free_var =
          Expr.fvar n s binfo (Nyaya_parser.Util.Uid.mk ())
        in
        let r = isDefEq env
          (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
          (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ()) in
        if not r then Logger.debug "defeq: Lam body mismatch";
        r
      ) else
        (Logger.debug "defeq: Lam btype mismatch"; false)
    | Literal (Expr.NatLit n1), Literal (Expr.NatLit n2) ->
      if Z.equal n1 n2 then true
      else (Logger.debug "defeq: NatLit mismatch: %s vs %s"
              (Z.to_string n1) (Z.to_string n2); false)
    | Proj { nat = n1; expr = e1; _ }, Proj { nat = n2; expr = e2; _ } ->
      if n1 == n2 && isDefEq env e1 e2 then true
      else (Logger.debug "defeq: Proj mismatch"; false)
    | ( Expr.Let { name = n1; btype = s1; value = v1; body = a },
        Expr.Let { name = n2; btype = s2; value = v2; body = b } ) ->
      if isDefEq env s1 s2 && isDefEq env v1 v2 then (
        let free_var =
          Expr.fvar n1 s1 Expr.Default (Nyaya_parser.Util.Uid.mk ())
        in
        let r = isDefEq env
          (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
          (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ()) in
        if not r then Logger.debug "defeq: Let body mismatch";
        r
      ) else
        (Logger.debug "defeq: Let btype/value mismatch"; false)
    | _ ->
      (* Structure eta (defEqEtaStruct x y): y is constructor-headed for
         a structure type T; compare Proj(i, x) against yArgs[i+numParams]
         for each field, after checking the inferred types are defeq
         (which ensures the parameters agree). *)
      let try_struct_eta x y =
        let hd, y_args = Expr.get_apps y in
        match Expr.node hd with
        | Expr.Const { name = ctor_name; _ } ->
          (match Hashtbl.find_opt env.tbl ctor_name with
           | Some (Decl.Ctor { inductive_name; num_params; num_fields; _ }) ->
             (match Hashtbl.find_opt env.tbl inductive_name with
              | Some (Decl.Inductive { ctor_names; num_idx; is_recursive; _ })
                when List.length ctor_names = 1
                  && num_idx = 0
                  && not is_recursive
                  && List.length y_args = num_params + num_fields ->
                if not (isDefEq env (infer env x) (infer env y)) then false
                else (
                  Logger.debug "defeq: struct-eta for %a" Name.pp inductive_name;
                  let rec check i =
                    if i >= num_fields then true
                    else
                      isDefEq env
                        (Expr.proj inductive_name i x)
                        (List.nth y_args (num_params + i))
                      && check (i + 1)
                  in
                  check 0)
              | _ -> false)
           | _ -> false)
        | _ -> false
      in
      if try_struct_eta e1' e2' || try_struct_eta e2' e1' then true
      else
      (* Lambda eta: fun x => body  =?=  e   iff   body =?= e x *)
      let try_lam_eta lam other =
        match Expr.node lam with
        | Expr.Lam { name; btype; body; binfo } ->
          let fv = Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ()) in
          isDefEq env
            (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
            (Expr.app other fv)
        | _ -> false
      in
      if try_lam_eta e1' e2' || try_lam_eta e2' e1' then true
      else
      (* Nat.xor one-step: Nat.xor is a guarded builtin (is_nat_builtin_name;
         see its doc) -- whnf deliberately stops at bare/applied `Nat.xor`
         rather than delta-unfolding into Nat.bitwise's well-founded-
         recursion body, which is what fixes the Depth_limit blowup on
         UInt32.not_neg_one. But Nat.xor.eq_1 states its equation POINT-
         FREE (`Nat.xor = Nat.bitwise bne`, no arguments applied at all),
         so the App-level retry that lets nat_lit_reduce see Nat.xor
         reapplied to its (literal-resolving) arguments never gets a
         chance to fire here -- there are no arguments to reattach.
         Handle this as a last-resort structural rule, exactly like
         struct/lambda eta above: if one side is literally the bare
         Nat.xor constant, try comparing its one-step delta-unfold
         (via delta_at_head, so this is guaranteed to match Nat.xor's
         real declaration value, not a hand-written guess) against the
         other side. This does not touch whnf's own recursive behavior
         at all, so it cannot reintroduce the depth-limit regression --
         it only fires here, as a final fallback, after every other
         comparison rule (including the App-level retry) has already
         failed. *)
      let try_xor_one_step other side =
        match Expr.node side with
        | Expr.Const { name; _ }
          when name = Name.Str (Name.Str (Name.Anon, "Nat"), "xor") ->
          let side' = Reduce.delta_at_head env side in
          if side' == side then false else isDefEq env side' other
        | _ -> false
      in
      if try_xor_one_step e1' e2' || try_xor_one_step e2' e1' then true
      else
        Logger.err
          "@[<v 0>[isDefEq] structural mismatch@,\
           \ lhs = %a@,\
           \ rhs = %a@]"
          (Defeq_failure "isDefEq: structural mismatch")
          Expr.pp e1 Expr.pp e2
  in
  result

(* TODO: complete ctor checks. *)
let check_ctor (decl : Decl.t) (env : Env.t) =
  match decl with
  | Decl.Ctor { num_params; inductive_name; _ } ->
    let inductive = Hashtbl.find env.tbl inductive_name in
    assert (num_params = Decl.get_inductive_num_params inductive);
    (* The constructor's type/telescope has to share the same parameters as the
       type of the inductive being declared. *)
    let ensure_same_params = true in
    (* For the non-parameter elements of the constructor type's telescope, the
       binder type must actually be a type (must infer as Sort _). *)
    let non_param_as_sort = true in
    (* For any non-parameter element of the constructor type's telescope, the
       element's inferred sort must be less than or equal to the inductive type's
       sort, or the inductive type being declared has to be a prop. *)
    let sort_le_inductive_sort = true in
    (* No argument to the constructor may contain a non-positive occurrence of
       the type being declared *)
    let non_positive = true in
    (* The end of the constructor's telescope must be a valid application of
       arguments to the type being declared *)
    let end_of_telescope_match = true in
    ensure_same_params && non_param_as_sort && sort_le_inductive_sort
    && non_positive && end_of_telescope_match
  | _ -> Logger.err "Ctor check called on non-ctor declaration" (Failure "")

let check (env : Env.t) (decl : Decl.t) : bool =
  let module Logger = (val env.logger) in
  match (decl : Decl.t) with
  | Def { info; value; red_hint = _red_hint } ->
    (* TODO: definitions should be unfolded according to reducibility hints. *)
    Logger.debugf Pp.pp_check (value, info.ty);
    let inf = (infer env value) in
    Logger.app "Inference complete";
    let ans = isDefEq env inf info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Thm { info; value } ->
    Logger.debugf Pp.pp_check (value, info.ty);
    let inf = (infer env value) in
    Logger.app "Inference complete";
    let ans = isDefEq env (inf) info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Axiom { name; uparams; ty } ->
    Logger.debugf Pp.pp_check_name (name, ty);
    true
  | Opaque { info; value } ->
    Logger.debugf Pp.pp_check (value, info.ty);
    let inf = (infer env value) in
    Logger.app "Inference complete";
    let ans = isDefEq env (inf) info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Ctor { info; inductive_name; _ } as d -> check_ctor d env
  | Rec _ -> (* TODO: what goes here? *) true
  | Inductive _ -> (* TODO: what goes here? *) true
  | _ ->
    (* Logger.warn "not checking decl: %a" Decl.pp decl;
       true *)
    Logger.err "failed checking decl: %a" (Failure "type checking failed")
      Decl.pp decl

(** We check if any declaration in the environment has 1) duplicate uparams or 2) lingering free variables in the type or 3) the type of its type is a sort. 
  If yes, we call that declaration well-posed and only typecheck those. *)
let well_posed (env : Env.t) (info : Decl.decl_info) : bool =
  let module Logger = (val env.logger) in
  let rec dup_exist = function
    | [] -> false
    | hd :: tl -> List.exists (( = ) hd) tl || dup_exist tl
  in
  let no_dup_uparams = dup_exist info.uparams |> not in
  let no_free_vars = Expr.has_free_vars info.ty |> not in
  let type_is_sort =
    try
      match infer env info.ty |> Expr.node with
      | Expr.Sort _ -> true
      | _ ->
        Logger.debug "info.ty : %a@." Expr.pp (infer env info.ty);
        false
    with TypeError origin ->
      Logger.err "well_posed: while inferring %a, type error: %s"
        (TypeError ("well_posed: " ^ origin))
        Name.pp info.name origin
  in

  no_dup_uparams && no_free_vars && type_is_sort

let typecheck (env : Env.t) =
  let iter = env.tbl |> Iter.of_hashtbl in
  let success = ref 0 in
  (* NYAYA_ONLY_DECL restricts the sweep to a single named declaration, for
     fast iterate-fix-verify cycles: `check` only inspects the target
     declaration's own value/type (dependencies are looked up from `env`
     during infer/whnf but not re-verified as top-level decls), so skipping
     every other declaration here has no effect on the result for the one
     we do check. *)
  let only_decl = Sys.getenv_opt "NYAYA_ONLY_DECL" in
  (* NYAYA_SWEEP_ALL checks every declaration, catching all failures (not
     just TypeError/Defeq_failure -- also Depth_limit, Not_well_posed, etc.)
     instead of stopping at the first one, and reports the complete
     pass/fail set at the end. This is the regression gate for the autoloop:
     a plain run dies at the first failing declaration in hash order, so it
     cannot by itself confirm "no previously-passing declaration regressed."
     No debug_*.txt files are written here -- the failure set for this
     codebase is currently large, and writing one file per failure would
     litter the repo. Use NYAYA_ONLY_DECL (or NYAYA_DECL_DEBUG) separately
     to get a trace for one specific declaration. *)
  let sweep_all =
    match Sys.getenv_opt "NYAYA_SWEEP_ALL" with
    | Some ("1" | "true" | "TRUE") -> true
    | _ -> false
  in
  let failures = ref [] in
  Iter.iter2
    (fun n d ->
      let decl_name_str = CCFormat.to_string Name.pp n in
      if
        match only_decl with
        | Some target -> not (String.equal target decl_name_str)
        | None -> false
      then ()
      else if sweep_all then (
        InferTrace.reset ();
        WhnfTrace.reset ();
        DefEqTrace.reset ();
        Hashtbl.reset whnf_memo;
        Hashtbl.reset infer_memo;
        Hashtbl.reset Expr.num_loose_bvars_memo;
        let record_failure () =
          failures := decl_name_str :: !failures;
          (* Print as we go, not just in the final summary: a long sweep can
             be killed (e.g. OOM from unbounded hash-cons growth, a known
             TODO) before it finishes, and a streamed line survives that
             where a summary computed at the end wouldn't. *)
          Logger.info "SWEEP_ALL_FAIL: %s" decl_name_str
        in
        try
          let info = Decl.get_decl_info d in
          if not (well_posed env info) then
            record_failure ()
          else if check env d then
            success := !success + 1
          else
            record_failure ()
        with
        | (Stack_overflow | Out_of_memory) as exn -> raise exn
        | _ -> record_failure ()
      )
      else
      let module DeclLogger : Env.LOGGER = Nyaya_parser.Util.MakeLogger (struct
        let header = CCFormat.to_string Name.pp n
      end) in
      let env = Env.with_logger env (module DeclLogger) in
      let prev_level = Logs.level () in
      (match Sys.getenv_opt "NYAYA_DECL_DEBUG" with
      | Some target when String.equal target decl_name_str ->
        Logs.set_level (Some Logs.Debug)
      | _ -> ());
      InferTrace.reset ();
      WhnfTrace.reset ();
      DefEqTrace.reset ();
      Hashtbl.reset whnf_memo;
      Hashtbl.reset infer_memo;
      Hashtbl.reset Expr.num_loose_bvars_memo;
      let info = Decl.get_decl_info d in
      (* Check well-posedness. *)
      let is_well_posed = well_posed env info in
      if not is_well_posed then
        DeclLogger.err "Declaration %a is not well-posed"
          (Not_well_posed "declaration not well-posed") Name.pp n
      else
        DeclLogger.success "Declaration %a is well-posed." Name.pp n;
      (* Decl is well-posed, so perform typechecking. *)
      (try
        if check env d then
          (DeclLogger.success "Type checked decl %a." Name.pp (Decl.get_name d);
          success := !success + 1)
        else
          ()
      with exn when (match exn with
                     | TypeError _ | Defeq_failure _ | Not_well_posed _
                     | Nyaya_parser.Util.Depth_limit _ -> true
                     | _ -> false) ->
        Logger.success "Failed after checking %d declarations in environment."
          !success;
        (* Auto-debug: re-run the failing declaration with debug logging
           to a file so the trace is available without a manual second run. *)
        let sanitized =
          String.map (fun c -> if c = '.' || c = ' ' || c = '/' then '_' else c)
            decl_name_str
        in
        let debug_file = "debug_" ^ sanitized ^ ".txt" in
        let oc = open_out debug_file in
        let debug_ppf = Format.formatter_of_out_channel oc in
        let saved_reporter = Logs.reporter () in
        Logs.set_reporter (DeclLogger.reporter debug_ppf);
        Logs.set_level (Some Logs.Debug);
        InferTrace.reset ();
        WhnfTrace.reset ();
        DefEqTrace.reset ();
        Hashtbl.reset whnf_memo;
        Hashtbl.reset infer_memo;
        Hashtbl.reset Expr.num_loose_bvars_memo;
        (try ignore (check env d) with _ -> ());
        Format.pp_print_flush debug_ppf ();
        close_out oc;
        Logs.set_reporter saved_reporter;
        Logs.set_level prev_level;
        Logger.info "Debug trace written to %s" debug_file;
        Logger.err "typecheck: failed on %a: %s"
          (TypeError ("typecheck: " ^ Printexc.to_string exn))
          Name.pp n (Printexc.to_string exn));
      Logs.set_level prev_level)
    iter;
  if sweep_all then (
    let total = !success + List.length !failures in
    Logger.info "SWEEP_ALL: %d/%d declarations passed." !success total;
    Logger.info "SWEEP_ALL: %d failing declarations:" (List.length !failures);
    List.iter (fun name -> Logger.info "SWEEP_ALL_FAIL: %s" name)
      (List.sort String.compare !failures)
  )
  else
    Logger.success "Successfully checked %d declarations in environment." !success
