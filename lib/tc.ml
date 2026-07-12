(* TODO: Refactor this whole module to replace manual match/error with Result.bind + projection helper. *)

[@@@warning "-27"]

exception TypeError of string

exception Defeq_failure of string

exception Not_well_posed of string

(* Raised when the kernel encounters a construct it does not yet support checking
   (e.g. an unimplemented reduction/computation rule). This is the "decline"
   signal: it is NOT a claim that the declaration is invalid -- it means we cannot
   verify it either way. The arena maps this to exit code 2 (declined), which is
   ignored for soundness/completeness scoring. Nothing raises it yet; it is the
   hook for deciding, decl-kind by decl-kind, what nyaya declines vs. checks. *)
exception Unsupported of string

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

  (* Convert a string literal to its constructor form, e.g. StrLit "ok" -> String.mk (List.cons ...). *)
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

  (* One-step iota reduction at the head of [e]: a recursor applied to a constructor-headed major premise. *)
  let iota_at_head (env : Env.t) (e : Expr.t) whnf infer isDefEq : Expr.t =
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
      | Decl.Rec { num_params; num_idx; num_motives; num_minors; rules; is_K; _ } ->
        (* The rule value's universe params get substituted with the actual application's levels. *)
        let decl_uparams =
          CCList.map Level.param (Decl.get_uparams decl)
        in
        let major_idx = num_params + num_idx + num_motives + num_minors in
        if List.length args <= major_idx then
          e
        else (
          let major0 = List.nth args major_idx in
          (* K-like reduction: replace a stuck major with its type's nullary constructor, guarded by defeq. *)
          let major =
            if not is_K then major0
            else
              let app_type = whnf env (infer env major0) in
              let ind_hd, ind_args = Expr.get_apps app_type in
              (match Expr.node ind_hd with
               | Expr.Const { name = ind_name; uparams = ind_levels } ->
                 (match Hashtbl.find_opt env.tbl ind_name with
                  | Some (Decl.Inductive { ctor_names = [ctor_name]; _ })
                    when List.exists
                           (fun (r : Decl.Rec_rule.t) -> r.ctor_name = ctor_name)
                           rules ->
                    let params = CCList.take num_params ind_args in
                    let cnstr_app =
                      Expr.mk_app (Expr.const ~ups:ind_levels ctor_name) params
                    in
                    let new_type = infer env cnstr_app in
                    let prev = Logs.level () in
                    Logs.set_level None;
                    let deq =
                      Fun.protect ~finally:(fun () -> Logs.set_level prev)
                        (fun () ->
                          try isDefEq env app_type new_type
                          with Defeq_failure _ -> false)
                    in
                    if deq then cnstr_app else major0
                  | _ -> major0)
               | _ -> major0)
          in
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
              (* Append only the constructor's own field args (not its params, already in prefix), or params get passed twice. *)
              let prefix_len = num_params + num_motives + num_minors in
              let prefix = CCList.take prefix_len args in
              (* Args after the major premise aren't part of the rule RHS; re-apply them to the result. *)
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
            (* NatLit major: treat as Nat.zero/Nat.succ and apply the matching rule directly. *)
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
            (* major isn't constructor-headed: try structure eta, projecting out each field. *)
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
      | Decl.Quot _ ->
        (* Quot.ind/Quot.lift computation rules. *)
        let quot_ind_name = Name.Str (Name.Str (Name.Anon, "Quot"), "ind") in
        let quot_lift_name = Name.Str (Name.Str (Name.Anon, "Quot"), "lift") in
        let quot_mk_name = Name.Str (Name.Str (Name.Anon, "Quot"), "mk") in
        let major_idx =
          if rec_name = quot_ind_name then Some 4
          else if rec_name = quot_lift_name then Some 5
          else None
        in
        (match major_idx with
        | None -> e
        | Some idx ->
          if List.length args <= idx then e
          else
            let major = whnf env (List.nth args idx) in
            let maj_hd, maj_args = Expr.get_apps major in
            (match Expr.node maj_hd with
            | Expr.Const { name; _ }
              when name = quot_mk_name && List.length maj_args = 3 ->
              let a = List.nth maj_args 2 in
              let f = List.nth args 3 in
              let suffix = CCList.drop (idx + 1) args in
              let red = Expr.mk_app f (a :: suffix) in
              Logger.debug "iota (quot): %a" Expr.pp red;
              red
            | _ -> e))
      | _ -> e)
    | _ -> e

  (* Builtin reductions for Nat literal ops; returns [Some result] if a reduction fired. *)
  let nat_lit_reduce (env : Env.t) (e : Expr.t) whnf : Expr.t option =
    let hd, args = Expr.get_apps e in
    match Expr.node hd with
    | Expr.Const { name; _ } ->
      let mk_name s1 s2 = Name.Str (Name.Str (Name.Anon, s1), s2) in
      (* Cheap, whnf-free test that an argument can never compute to a literal,
         used to bail out of a Nat op before forcing the other argument. *)
      let obviously_not_lit e =
        match Expr.node e with
        | Expr.FreeVar _ | Expr.Lam _ | Expr.Forall _ | Expr.Sort _ -> true
        | _ -> false
      in
      (* Recover a concrete literal, looking through [Nat.succ] chains; returns
         [None] on a symbolic argument rather than forcing it. *)
      let rec as_nat_lit e =
        if obviously_not_lit e then None
        else
        let e = whnf env e in
        match Expr.node e with
        | Expr.Literal (Expr.NatLit n) -> Some n
        | Expr.App _ -> (
          match Expr.get_apps e with
          | shd, [ y ] -> (
            match Expr.node shd with
            | Expr.Const { name = sn; _ } when sn = mk_name "Nat" "succ" -> (
              match as_nat_lit y with
              | Some k -> Some (Z.succ k)
              | None -> None)
            | _ -> None)
          | _ -> None)
        | _ -> None
      in
      let bool_const b =
        Expr.const (mk_name "Bool" (if b then "true" else "false"))
      in
      let binary f =
        match args with
        (* Bail before forcing if either argument obviously isn't a literal. *)
        | [ a; b ] when obviously_not_lit a || obviously_not_lit b -> None
        | [a; b] ->
          (match as_nat_lit a, as_nat_lit b with
          | Some m, Some n -> Some (f m n)
          | _ -> None)
        | _ -> None
      in
      (match name with
      | n when n = mk_name "Nat" "succ" ->
        (* Fold into a literal only when the argument is already one; [succ] is a
           constructor, so [succ x] with symbolic [x] is already in whnf. *)
        (match args with
        | [ a ] -> (
          match Expr.node a with
          | Expr.Literal (Expr.NatLit n) -> Some (Expr.natlit (Z.succ n))
          | _ -> None)
        | _ -> None)
      | n when n = mk_name "Nat" "add" ->
        (match binary (fun m n -> Expr.natlit (Z.add m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota for Nat.add with a symbolic/successor-headed second arg (NatLit case bounded to n <= 64). *)
          match args with
          | [a; b] ->
            (match as_nat_lit b with
            | Some n when Z.leq n (Z.of_int 64) ->
              if Z.equal n Z.zero then Some a
              else
                let inner = Expr.mk_app hd [a; Expr.natlit (Z.pred n)] in
                Some (Expr.mk_app (Expr.const (mk_name "Nat" "succ")) [inner])
            | _ ->
              (* Symbolic successor: Nat.add x (Nat.succ y) -> Nat.succ (Nat.add x y). *)
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
          (* Partial iota for Nat.sub: recurses only on the subtrahend, matching Nat.sub's own equations. *)
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
          (* Partial iota for Nat.mul with a symbolic/successor-headed second arg (NatLit case bounded to n <= 64). *)
          match args with
          | [a; b] ->
            (match as_nat_lit b with
            | Some n when Z.leq n (Z.of_int 64) ->
              if Z.equal n Z.zero then Some (Expr.natlit Z.zero)
              else
                let inner = Expr.mk_app hd [a; Expr.natlit (Z.pred n)] in
                Some (Expr.mk_app (Expr.const (mk_name "Nat" "add")) [inner; a])
            | _ ->
              (* Symbolic successor: Nat.mul x (Nat.succ y) -> Nat.add (Nat.mul x y) x. *)
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
        (match binary (fun m n -> Expr.natlit (Z.pow m (Z.to_int n))) with
        | Some _ as r -> r
        | None ->
          (* Partial iota for Nat.pow: recurses only on the exponent (NatLit case bounded to n <= 64). *)
          match args with
          | [a; b] ->
            (match as_nat_lit b with
            | Some n when Z.leq n (Z.of_int 64) ->
              if Z.equal n Z.zero then Some (Expr.natlit Z.one)
              else
                let inner = Expr.mk_app hd [a; Expr.natlit (Z.pred n)] in
                Some (Expr.mk_app (Expr.const (mk_name "Nat" "mul")) [inner; a])
            | _ ->
              (* Symbolic successor: Nat.pow m (Nat.succ y) -> Nat.mul (Nat.pow m y) m. *)
              let b' = whnf env b in
              let bhd, bargs = Expr.get_apps b' in
              (match Expr.node bhd, bargs with
               | Expr.Const { name = sn; _ }, [y]
                 when sn = mk_name "Nat" "succ" ->
                 let inner = Expr.mk_app hd [a; y] in
                 Some (Expr.mk_app (Expr.const (mk_name "Nat" "mul")) [inner; a])
               | _ -> None))
          | _ -> None)
      | n when n = mk_name "Nat" "div" ->
        (match binary (fun m n ->
          if Z.equal n Z.zero then Expr.natlit Z.zero
          else Expr.natlit (Z.div m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota for Nat.div: 0 with either symbolic arg zero. *)
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
          (* Divisor 0 returns the dividend, not 0 (Nat.mod's own doc comment: "5 % 0 = 5"). *)
          if Z.equal n Z.zero then Expr.natlit m
          else Expr.natlit (Z.rem m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota for Nat.mod, including the Fin-literal case (Nat.ble proves m > n). *)
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
            | _ ->
              (* Succ-headed dividend: one delta step exposes the ite that this whnf's further. *)
              let a' = whnf env a in
              let ahd, aargs = Expr.get_apps a' in
              (match Expr.node ahd, aargs with
               | Expr.Const { name = sn; _ }, [_]
                 when sn = mk_name "Nat" "succ" ->
                 let unfolded = delta_at_head env hd in
                 if unfolded != hd then Some (Expr.mk_app unfolded args)
                 else None
               | _ -> None))
          | _ -> None))
      | n when n = mk_name "Nat" "beq" ->
        (match binary (fun m n -> bool_const (Z.equal m n)) with
        | Some _ as r -> r
        | None ->
          (* Partial iota for Nat.beq: peel matching Nat.succ off both sides, else zero-vs-succ is false. *)
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
          (* Partial iota for Nat.ble: peel matching Nat.succ off both sides, else zero-vs-succ decides it. *)
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
        binary (fun m n -> Expr.natlit (Z.logxor m n))
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
    (* infer Lam: Pi binder (abstract (infer (instantiate body binderFvar)) binderFvar). *)
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
    (* infer Forall: imax(sortOf binder, sortOf (instantiate body (fvar binder))). *)
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
    (* infer Const: the declared type with its own uparams substituted by this application's levels. *)
    let known_type : Decl.t =
      match Hashtbl.find_opt env.tbl name with
      | Some d -> d
      | None ->
        (* Referencing a constant absent from the environment is a type error,
           not a nyaya bug. Raise a [TypeError] (so the verdict is [Reject])
           rather than let a [Not_found] escape as a checker error. *)
        Logger.err "infer Const: unknown constant %a"
          (TypeError "infer Const: unknown constant") Name.pp name
    in
    let known_type_uparams =
      CCList.map Level.param (Decl.get_uparams known_type)
    in
    Expr.subst_levels (known_type |> Decl.get_type) known_type_uparams uparams
  | Expr.App (f, arg) ->
    (* infer App: whnf(infer f) must be a Pi; check binder.type =?= infer arg, instantiate body with arg. *)
    (match whnf env (infer env f) |> Expr.node with
    | Expr.Forall { btype; body; _ } ->
      (* The argument's type must be defeq to the parameter type -- checked
         unconditionally, matching the kernel's [infer_app]. (Proof irrelevance
         lives in [isDefEq], not here: it never licenses skipping this check.) *)
      begin
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
    (* infer Let: check binder.type is a sort and infer(val) =?= binder.type, then infer (instantiate body val). *)
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
    (* infer Proj: instantiate the sole constructor's type with the struct's params, then with each prior projection, up to nat. *)
    let struct_type = infer env expr |> whnf env in
    let const, ty_args = Expr.get_apps struct_type in
    (match const |> Expr.node with
    | Const { name; uparams } ->
      let inductive_info = Hashtbl.find env.tbl name in
      let ctor_names = Decl.get_inductive_ctors inductive_info in
      let ctor_num_params = Decl.get_inductive_num_params inductive_info in
      (* A projection is only valid when its type is a structure: an inductive
         with exactly one constructor. Raise a [TypeError] (verdict [Reject])
         otherwise, rather than crashing on a multi-constructor inductive. *)
      if CCList.length ctor_names <> 1 then
        Logger.err
          "infer Proj: type %a is not a single-constructor structure (%d \
           constructors)"
          (TypeError "infer Proj: type is not a single-constructor structure")
          Name.pp name
          (CCList.length ctor_names);
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
      (* Projecting out of a proposition is only sound for proof (Prop-typed)
         fields: proof irrelevance equates all inhabitants of the structure, so
         a data projection could equate distinct data. A field is also rejected
         when an earlier depended-upon field is data. Non-Prop structures are
         unrestricted. *)
      let is_prop t =
        match Expr.node (whnf env (infer env t)) with
        | Expr.Sort u -> Level.is_zero u
        | _ -> false
      in
      let is_prop_type = is_prop struct_type in
      (* Now, instantiate the projections *)
      for i = 0 to nat - 1 do
        let for_ty = whnf env !ctor_type in
        match for_ty |> Expr.node with
        | Forall { btype; body; _ } ->
          (* A field that a later field depends on must itself be a proof. *)
          if is_prop_type && Expr.num_loose_bvars body > 0 && not (is_prop btype)
          then
            Logger.err
              "infer Proj: projection out of a proposition depends on data field %d"
              (TypeError "infer Proj: projection out of a proposition depends on data")
              i;
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
      | Forall { btype; _ } ->
        (* The projected field itself must be a proof when the structure is. *)
        if is_prop_type && not (is_prop btype) then
          Logger.err "infer Proj: data projection out of a proposition"
            (TypeError "infer Proj: data projection out of a proposition");
        btype
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
    (* Locally nameless: a bound variable here means a binder wasn't instantiated with a free var. *)
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
    (* Try Nat literal builtins before delta, so e.g. Nat.add on two NatLits computes in O(1) instead of O(n) iota. *)
    (match Reduce.nat_lit_reduce env expr whnf with
    | Some r ->
      Logger.debug "whnf: nat-builtin";
      whnf env r
    | None ->
    let hd' =
      match hd |> Expr.node with
      | Expr.Const _ -> Reduce.delta_at_head env hd
      | _ -> whnf env hd
    in
    let e1 = Expr.mk_app hd' args in
    let e2 = Reduce.beta e1 in

    (* Now attempt iota at head *)
    let e3 = Reduce.iota_at_head env e2 whnf infer isDefEq |> Reduce.beta in
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
    (* Normalize Nat.zero to NatLit 0 so isDefEq doesn't need a Const-vs-Literal case. *)
    if name = Name.Str (Name.Str (Name.Anon, "Nat"), "zero") then
      Expr.natlit Z.zero
    else
      let e' = Reduce.delta_at_head env expr in
      if e' == expr then expr
      else (Logger.debug "whnf Const: delta"; whnf env e')
  | Expr.Forall _ ->
    (* Already in whnf: the head is a Forall constructor, don't reduce under binders. *)
    expr
  | Expr.Proj {name; nat; expr = inner} ->
      let inner' = whnf env inner in
      (* String literals aren't constructor-headed; expand to String.mk first so proj-reduce can fire. *)
      let inner' =
        match Expr.node inner' with
        | Expr.Literal (Expr.StrLit s) -> Reduce.string_lit_to_ctor s
        | _ -> inner'
      in
      let (hd, args) = Expr.get_apps inner' in
      (match Expr.node hd with
       | Expr.Const { name = cname; _ } ->
         (* Only reduce if the head is actually a constructor of the projected type, not a stuck recursor. *)
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

(* TODO: optimize def eq checking with union-find; audit for other wasteful whnfs before def eq check. *)
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
  (* Proof irrelevance, before whnf and before the same-head-args shortcut below (else that shortcut deep-unfolds proof data first). *)
  let is_prop ty =
    match Expr.node (whnf env (infer env ty)) with
    | Expr.Sort u -> Level.is_zero u
    | _ -> false
  in
  let s = infer env e1 in
  let t = infer env e2 in
  if is_prop s && is_prop t && isDefEq env s t then true
  else
  (* Same-head-args shortcut, before whnf: if both sides are the same Const applied to equal-arity args, compare args first. *)
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
      (* Speculative: suppress logging and treat a failed/raising comparison as "shortcut doesn't apply", not a real failure. *)
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
  (* Structure eta: y is constructor-headed for a structure type T; compare Proj(i, x) against each of y's fields. *)
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
                    (CCList.nth y_args (num_params + i))
                  && check (i + 1)
              in
              check 0)
          | _ -> false)
       | _ -> false)
    | _ -> false
  in
  (* Unit-like defeq: any two terms of a non-recursive, single-ctor, zero-field structure type are defeq once their types agree. *)
  let is_def_eq_unit_like x y =
    let x_type = whnf env (infer env x) in
    let hd, _ = Expr.get_apps x_type in
    match Expr.node hd with
    | Expr.Const { name = ind_name; _ } ->
      (match Hashtbl.find_opt env.tbl ind_name with
       | Some (Decl.Inductive { ctor_names = [ctor_name]; num_idx = 0; is_recursive = false; _ }) ->
         (match Hashtbl.find_opt env.tbl ctor_name with
          | Some (Decl.Ctor { num_fields = 0; _ }) -> isDefEq env x_type (infer env y)
          | _ -> false)
       | _ -> false)
    | _ -> false
  in
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
  (* Nat.xor is delta-guarded; as a last resort try one manual delta step of it. *)
  let try_xor_one_step other side =
    match Expr.node side with
    | Expr.Const { name; _ }
      when name = Name.Str (Name.Str (Name.Anon, "Nat"), "xor") ->
      let side' = Reduce.delta_at_head env side in
      if side' == side then false else isDefEq env side' other
    | _ -> false
  in
  (* Bridge [NatLit n] against a [Nat.succ] chain: peel leading [succ]s in a flat
     loop, counting, then match [n - count] against the base. *)
  let try_natlit_succ lit_side other =
    let succ_arg e =
      match Expr.get_apps e with
      | shd, [ y ]
        when (match Expr.node shd with
             | Expr.Const { name; _ } ->
               name = Name.Str (Name.Str (Name.Anon, "Nat"), "succ")
             | _ -> false) ->
        Some y
      | _ -> None
    in
    match Expr.node lit_side with
    | Expr.Literal (Expr.NatLit n) -> (
      let rec loop count cur =
        if Z.gt count n then false (* more [succ]s than [n]: cannot be equal *)
        else
          match succ_arg cur with
          | Some y -> loop (Z.succ count) (whnf env y)
          | None -> isDefEq env (Expr.natlit (Z.sub n count)) cur
      in
      match succ_arg other with
      | Some _ -> loop Z.zero other
      | None -> false)
    | _ -> false
  in
  (* Shared final fallback, reached from the catch-all below and from a FreeVar/FreeVar id mismatch. *)
  let final_fallback e1' e2' =
    if try_struct_eta e1' e2' || try_struct_eta e2' e1' then true
    else if is_def_eq_unit_like e1' e2' || is_def_eq_unit_like e2' e1' then true
    else if try_lam_eta e1' e2' || try_lam_eta e2' e1' then true
    else if try_xor_one_step e1' e2' || try_xor_one_step e2' e1' then true
    else if try_natlit_succ e1' e2' || try_natlit_succ e2' e1' then true
    else
      Logger.err
        "@[<v 0>[isDefEq] structural mismatch@,\
         \ lhs = %a@,\
         \ rhs = %a@]"
        (Defeq_failure "isDefEq: structural mismatch")
        Expr.pp e1 Expr.pp e2
  in
  let result =
    match Expr.node e1', Expr.node e2' with
    | Expr.Sort u1, Expr.Sort u2 ->
      if Level.(u1 === u2) then true
      else (Logger.debug "defeq: Sort level mismatch: %a vs %a"
              Level.pp u1 Level.pp u2; false)
    | Expr.FreeVar { fvarId = f1; _ }, Expr.FreeVar { fvarId = f2; _ } ->
      if f1 = f2 then true
      else (Logger.debug "defeq: FreeVar id mismatch, trying final fallback";
            final_fallback e1' e2')
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
      (* Try per-argument congruence first (suppressing a nested raise); only then fall back to struct-eta on the whole terms. *)
      let try_cong () =
        let prev = Logs.level () in
        Logs.set_level None;
        Fun.protect ~finally:(fun () -> Logs.set_level prev)
          (fun () ->
            try
              isDefEq env (Reduce.delta_at_head env f) (Reduce.delta_at_head env g)
              && isDefEq env a b
            with Defeq_failure _ -> false)
      in
      if try_cong () then true
      else if try_struct_eta e1' e2' || try_struct_eta e2' e1' then true
      else (Logger.debug "defeq: App fn/arg mismatch (and struct-eta failed)"; false)
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
    | _ -> final_fallback e1' e2'
  in
  result

(* Validate a constructor: its parameters must match the inductive's, each field
   must live in a universe no larger than the inductive's (unless the inductive
   is a Prop), and its result must apply the inductive to those parameters and
   its indices. Strict positivity of the inductive's occurrences is not yet
   checked -- see doc/soundness-risks.md #3. *)
let check_ctor (decl : Decl.t) (env : Env.t) =
  let module Logger = (val env.logger) in
  match decl with
  | Decl.Ctor { info = ctor_info; inductive_name; num_params; _ } ->
    let inductive = Hashtbl.find env.tbl inductive_name in
    let ind_info = Decl.get_decl_info inductive in
    let num_idx =
      match inductive with Decl.Inductive { num_idx; _ } -> num_idx | _ -> 0
    in
    let num_nested =
      match inductive with
      | Decl.Inductive { num_nested; _ } -> num_nested
      | _ -> 0
    in
    assert (num_params = Decl.get_inductive_num_params inductive);
    (* Peel the inductive's arity to recover the shared parameter free variables
       and its resultant universe level. *)
    let param_fvars = ref [] in
    let rec peel_ind k ty =
      if k <= 0 then ty
      else
        match Expr.node (whnf env ty) with
        | Expr.Forall { name; btype; binfo; body } ->
          let fv = Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ()) in
          if List.length !param_fvars < num_params then
            param_fvars := !param_fvars @ [ fv ];
          peel_ind (k - 1)
            (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
        | _ -> ty
    in
    let ind_result = peel_ind (num_params + num_idx) ind_info.ty in
    let result_level =
      match Expr.node (whnf env ind_result) with
      | Expr.Sort l -> l
      | _ -> Level.Zero
    in
    let params_arr = Array.of_list !param_fvars in
    let param_type i =
      match Expr.node params_arr.(i) with
      | Expr.FreeVar { expr; _ } -> expr
      | _ -> assert false
    in
    (* Whether [e] mentions the inductive being defined; free variables and
       other atoms are leaves. *)
    let rec has_ind_occ (e : Expr.t) : bool =
      match Expr.node e with
      | Expr.Const { name; _ } -> name = inductive_name
      | Expr.App (f, a) -> has_ind_occ f || has_ind_occ a
      | Expr.Lam { btype; body; _ } | Expr.Forall { btype; body; _ } ->
        has_ind_occ btype || has_ind_occ body
      | Expr.Let { btype; value; body; _ } ->
        has_ind_occ btype || has_ind_occ value || has_ind_occ body
      | Expr.Proj { expr; _ } -> has_ind_occ expr
      | Expr.BoundVar _ | Expr.FreeVar _ | Expr.Sort _ | Expr.Literal _ -> false
    in
    (* Whether [t] applies the inductive (with its own universe parameters) to
       exactly the shared parameter fvars and its indices, with no index
       mentioning the inductive. Universe parameters are compared structurally by
       name, since [Name.t] is not interned. *)
    let is_valid_ind_app t =
      let head, args = Expr.get_apps t in
      let head_ok =
        match Expr.node head with
        | Expr.Const { name; uparams } ->
          name = inductive_name
          && (try
                List.for_all2
                  (fun lvl nm ->
                    match lvl with Level.Param p -> p = nm | _ -> false)
                  uparams ind_info.uparams
              with Invalid_argument _ -> false)
        | _ -> false
      in
      head_ok
      && List.length args = num_params + num_idx
      &&
      let params_args, index_args = CCList.take_drop num_params args in
      (try List.for_all2 (fun a p -> a == p) params_args !param_fvars
       with Invalid_argument _ -> false)
      && not (List.exists has_ind_occ index_args)
    in
    (* Strict positivity of a field's type: the inductive may occur only in the
       result of a function type (never to the left of an arrow) and only as a
       plain recursive application. Reduces to head-normal form but no further, so
       an occurrence that could be reduced away still counts. Only sound for
       inductives with no nested occurrences: a nested one (e.g. a field of type
       [List I]) is rejected by the final case here, but the real kernel un-nests
       it into a fresh parameter first, which nyaya does not do -- so the check is
       gated on [num_nested = 0] below. *)
    let rec check_positivity t =
      let t = whnf env t in
      if not (has_ind_occ t) then ()
      else
        match Expr.node t with
        | Expr.Forall { name; btype = dom; binfo; body } ->
          if has_ind_occ dom then
            Logger.err
              "check_ctor: non-positive occurrence of %a in a field of %a"
              (TypeError "constructor has a non-positive occurrence")
              Name.pp inductive_name Name.pp ctor_info.name;
          let fv = Expr.fvar name dom binfo (Nyaya_parser.Util.Uid.mk ()) in
          check_positivity
            (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
        | _ ->
          if not (is_valid_ind_app t) then
            Logger.err
              "check_ctor: invalid occurrence of %a in a field of %a"
              (TypeError "constructor has an invalid recursive occurrence")
              Name.pp inductive_name Name.pp ctor_info.name
    in
    (* Peel the constructor's telescope without reducing it -- the type must be
       manifest foralls, so a type that merely reduces to the inductive is still
       rejected. Parameters are def-eq-checked against the inductive's; fields
       are universe-checked. *)
    let rec loop i t =
      match Expr.node t with
      | Expr.Forall { name; btype = dom; binfo; body } ->
        let fv =
          if i < num_params then begin
            if not (isDefEq env dom (param_type i)) then
              Logger.err
                "check_ctor: parameter %d of %a does not match the inductive's"
                (TypeError "constructor parameter does not match inductive")
                (i + 1) Name.pp ctor_info.name;
            params_arr.(i)
          end
          else begin
            (* field sort <= inductive level, or the inductive is a Prop. *)
            let s = infer_sort_of env dom in
            if not (Level.(s <= result_level) || Level.is_zero result_level)
            then
              Logger.err
                "check_ctor: field %d of %a has a universe too big for the \
                 inductive"
                (TypeError "constructor field universe too big")
                (i + 1) Name.pp ctor_info.name;
            if num_nested = 0 then check_positivity dom;
            Expr.fvar name dom binfo (Nyaya_parser.Util.Uid.mk ())
          end
        in
        loop (i + 1)
          (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
      | _ ->
        if is_valid_ind_app t then true
        else
          Logger.err "check_ctor: invalid return type for %a"
            (TypeError "constructor has an invalid return type")
            Name.pp ctor_info.name
    in
    loop 0 ctor_info.ty
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
    (* A theorem's type must be a proposition (unlike a definition's). *)
    (match Expr.node (whnf env (infer env info.ty)) with
    | Expr.Sort u when Level.is_zero u -> ()
    | _ ->
      Logger.err "check Thm: theorem type %a is not a proposition"
        (TypeError "theorem type is not a proposition") Expr.pp info.ty);
    let inf = (infer env value) in
    Logger.app "Inference complete";
    let ans = isDefEq env (inf) info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Axiom { name; uparams; ty } ->
    Logger.debugf Pp.pp_check_name (name, ty);
    true
  | Quot { info } ->
    (* Primitive, like Axiom: no value to check. *)
    Logger.debugf Pp.pp_check_name (info.name, info.ty);
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
  | Inductive { info; num_params; num_idx; _ } ->
    (* An inductive's type must be an arity: a Pi-telescope of exactly
       [num_params] parameters followed by [num_idx] indices, ending in a sort. *)
    let rec peel k ty =
      match Expr.node (whnf env ty) with
      | Expr.Forall { name; btype; binfo; body } when k > 0 ->
        let fv = Expr.fvar name btype binfo (Nyaya_parser.Util.Uid.mk ()) in
        peel (k - 1)
          (Expr.instantiate ~logger:env.logger ~free_var:fv ~expr:body ())
      | Expr.Sort _ when k = 0 -> true
      | _ -> false
    in
    if peel (num_params + num_idx) info.ty then true
    else
      Logger.err
        "check Inductive: type of %a is not an arity of %d parameter(s) + %d \
         index(es) ending in a sort"
        (TypeError "inductive type is not a valid arity")
        Name.pp info.name num_params num_idx

(* Well-posed: no duplicate uparams, no free vars in the type, and the type's type is a sort. *)
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

(* The three verdicts the Lean Kernel Arena understands, one per export file.
   [Accept]  -> exit 0: every declaration checked out.
   [Reject]  -> exit 1: at least one declaration is provably invalid.
   [Decline] -> exit 2: we hit a construct we don't support and can't judge the
                file either way (ignored by the arena for scoring).
   The [string] carries the offending declaration + reason, for diagnostics. *)
type verdict = Accept | Reject of string | Decline of string

(* Produce a single arena verdict for a whole environment (one export file).

   Precedence is [Reject] > [Decline] > [Accept], and it is sound in the sense
   that matters: we never return [Accept] unless *every* declaration fully
   checked. A concretely-detected invalidity (a failed [check], a type/defeq
   error, or a malformed declaration) dominates -- it makes the file invalid
   regardless of any unsupported declarations elsewhere -- so we short-circuit
   on the first reject. An [Unsupported] declaration only downgrades an
   otherwise-clean file to [Decline]; it never turns into a false accept.

   [Stack_overflow]/[Out_of_memory] and any other unexpected exception are
   deliberately *not* caught here: they propagate so the entry point can map
   them to a checker-error exit code (not 0/1/2), matching the arena's
   "anything else = a bug in the checker" rule. *)
let check_env_verdict (env : Env.t) : verdict =
  let exception Found_reject of string in
  let declined = ref None in
  let iter = env.tbl |> Iter.of_hashtbl in
  (* A name declared more than once makes the file invalid. *)
  match env.duplicates with
  | dup :: _ ->
    Reject (CCFormat.sprintf "%a: duplicate declaration name" Name.pp dup)
  | [] ->
  try
    Iter.iter2
      (fun n d ->
        (* Same per-declaration memo reset the discovery sweep does, so one
           declaration's traces/caches don't bleed into the next. *)
        InferTrace.reset ();
        WhnfTrace.reset ();
        DefEqTrace.reset ();
        Hashtbl.reset whnf_memo;
        Hashtbl.reset infer_memo;
        Hashtbl.reset Expr.num_loose_bvars_memo;
        let name = CCFormat.to_string Name.pp n in
        try
          let info = Decl.get_decl_info d in
          if not (well_posed env info) then
            raise (Found_reject (name ^ ": declaration not well-posed"))
          else if not (check env d) then
            raise (Found_reject (name ^ ": type mismatch"))
        with
        | Unsupported msg ->
          if !declined = None then declined := Some (name ^ ": " ^ msg)
        | TypeError m | Defeq_failure m | Not_well_posed m ->
          raise (Found_reject (name ^ ": " ^ m))
        | Nyaya_parser.Util.Depth_limit m ->
          raise (Found_reject (name ^ ": depth limit: " ^ m)))
      iter;
    (match !declined with Some m -> Decline m | None -> Accept)
  with Found_reject m -> Reject m

let typecheck (env : Env.t) =
  let iter = env.tbl |> Iter.of_hashtbl in
  let total = Hashtbl.length env.tbl in
  let success = ref 0 in
  let skipped = ref 0 in
  (* NYAYA_ONLY_DECL restricts the sweep to one named declaration, for fast iterate-fix-verify cycles. *)
  let only_decl = Sys.getenv_opt "NYAYA_ONLY_DECL" in
  (* NYAYA_SKIP_PREFIX excludes declarations by namespace prefix (comma-separated) from a discovery walk. *)
  let skip_prefixes =
    match Sys.getenv_opt "NYAYA_SKIP_PREFIX" with
    | Some s ->
      String.split_on_char ',' s
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    | None -> []
  in
  let is_skipped decl_name_str =
    List.exists
      (fun prefix ->
        String.equal decl_name_str prefix
        ||
        let plen = String.length prefix in
        String.length decl_name_str > plen
        && String.equal (String.sub decl_name_str 0 (plen + 1)) (prefix ^ "."))
      skip_prefixes
  in
  (* NYAYA_SWEEP_ALL checks every declaration and reports the full pass/fail set, instead of stopping at the first failure. *)
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
      else if only_decl = None && is_skipped decl_name_str then (
        skipped := !skipped + 1;
        Logger.info "SKIPPED (NYAYA_SKIP_PREFIX): %s" decl_name_str)
      else if sweep_all then (
        InferTrace.reset ();
        WhnfTrace.reset ();
        DefEqTrace.reset ();
        Hashtbl.reset whnf_memo;
        Hashtbl.reset infer_memo;
        Hashtbl.reset Expr.num_loose_bvars_memo;
        let record_failure () =
          failures := decl_name_str :: !failures;
          (* Stream failures as they happen, so a killed sweep still leaves a partial record. *)
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
        let remaining = total - !success - !skipped - 1 in
        Logger.success
          "Failed after checking %d declarations (%d skipped) in \
           environment; %d declarations remaining."
          !success !skipped remaining;
        (* Auto-debug: re-run the failing declaration with debug logging to a file. *)
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
    let total_checked = !success + List.length !failures in
    Logger.info "SWEEP_ALL: %d/%d declarations passed (%d skipped)." !success
      total_checked !skipped;
    Logger.info "SWEEP_ALL: %d failing declarations:" (List.length !failures);
    List.iter (fun name -> Logger.info "SWEEP_ALL_FAIL: %s" name)
      (List.sort String.compare !failures)
  )
  else
    Logger.success "Successfully checked %d declarations in environment." !success
