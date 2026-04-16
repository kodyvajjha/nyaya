(* Core checker: type inference, weak-head normalisation, and
   definitional equality.

   These three procedures are mutually recursive and therefore share a
   module.  Each takes a [Ctx.t] — the checker's bundle of operational
   services (logger, memos, fvar generator, tracers) — and produces a
   pure result (or raises one of the checker exceptions).  No
   module-level mutable state is touched from here. *)

exception TypeError of string

exception Defeq_failure of string

module Pp = struct
  let pp_inferring fpf expr =
    CCFormat.fprintf fpf "@[<hov 0>Now inferring @[<hov 2>%a@]@]" Expr.pp expr

  let pp_failed_inferring fpf expr =
    CCFormat.fprintf fpf "@[<v 0>failed inferring:@, @[<hov 2> %a@]@]" Expr.pp
      expr
end

(* Enter / leave helpers keep the [try/with] dance in one place. *)
let with_trace
    ~enter ~leave_success ~leave_failure ~memo ~key input thunk =
  match Memo.find_opt memo key with
  | Some v -> v
  | None ->
    let frame = enter input in
    (match thunk () with
    | v ->
      leave_success frame v;
      Memo.add memo key v;
      v
    | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      leave_failure frame exn;
      Printexc.raise_with_backtrace exn bt)

let rec infer (ctx : Ctx.t) (expr : Expr.t) : Expr.t =
  with_trace
    ~enter:(fun input -> Ctx.Infer_trace.enter ctx.infer_trace ctx.logger input)
    ~leave_success:(fun frame out ->
      Ctx.Infer_trace.leave_success ctx.infer_trace ctx.logger frame out)
    ~leave_failure:(fun frame exn ->
      Ctx.Infer_trace.leave_failure ctx.infer_trace ctx.logger frame exn)
    ~memo:ctx.infer_memo ~key:(Expr.tag expr) expr (fun () -> infer_impl ctx expr)

and infer_impl (ctx : Ctx.t) (expr : Expr.t) : Expr.t =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  match Expr.node expr with
  | Expr.Sort u -> Expr.sort (Level.Succ u)
  | Expr.FreeVar { expr; _ } -> expr
  | Expr.Lam { name; btype; binfo; body } ->
    (match whnf ctx (infer ctx btype) |> Expr.node with
    | Expr.Sort _ ->
      let binder_free_var = Ctx.fresh_fvar ctx ~name ~btype ~binfo in
      let body_type =
        infer ctx (Expr.instantiate ~free_var:binder_free_var ~expr:body)
      in
      let target_id = Expr.get_fvar_id binder_free_var in
      Expr.pi name btype binfo (Expr.abstract_fvar ~target_id ~k:0 body_type)
    | _ ->
      L.err "infer Lam: binder type is not a sort: %a"
        (TypeError "infer Lam: binder type not a sort")
        Expr.pp expr)
  | Expr.Forall { name; btype; binfo; body } ->
    let l = infer_sort_of ctx btype in
    let free_var = Ctx.fresh_fvar ctx ~name ~btype ~binfo in
    let r = infer_sort_of ctx (Expr.instantiate ~free_var ~expr:body) in
    Expr.sort (Level.IMax (l, r) |> Level.simplify)
  | Expr.Const { name; uparams } ->
    let known_type : Decl.t = Env.find (Ctx.env ctx) name in
    let known_type_uparams =
      CCList.map Level.param (Decl.get_uparams known_type)
    in
    Expr.subst_levels (known_type |> Decl.get_type) known_type_uparams uparams
  | Expr.App (f, arg) ->
    (match whnf ctx (infer ctx f) |> Expr.node with
    | Expr.Forall { btype; body; _ } ->
      let btype_is_prop =
        match Expr.node (whnf ctx (infer ctx btype)) with
        | Expr.Sort u -> Level.is_zero u
        | _ -> false
      in
      if not btype_is_prop then begin
        let arg_type = infer ctx arg in
        if not (is_def_eq ctx btype arg_type) then
          L.err
            "@[<v 0>[infer App] defeq failed for@,\
             \ expr = %a@,\
             \ btype = %a@,\
            \ arg_type = %a@]"
            (Defeq_failure "infer App: btype vs arg type")
            Expr.pp expr Expr.pp btype Expr.pp arg_type
      end;
      Expr.instantiate ~free_var:arg ~expr:body
    | _ ->
      L.err "infer App: expected forall, got @[%a@]"
        (TypeError "infer App: whnf of fn type not a forall")
        Expr.pp expr)
  | Let { btype; value; body; _ } ->
    (match infer ctx btype |> Expr.node with
    | Sort _ ->
      if not (is_def_eq ctx btype (infer ctx value)) then
        L.err
          "@[<v 0>[infer Let] defeq failed for@,\
           \ btype = %a@,\
           \ value_type = %a@]"
          (Defeq_failure "infer Let: btype vs value type")
          Expr.pp btype Expr.pp (infer ctx value);
      infer ctx (Expr.instantiate ~free_var:value ~expr:body)
    | _ ->
      L.err "infer Let: binder type is not a sort: %a"
        (TypeError "infer Let: binder type not a sort")
        Expr.pp expr)
  | Proj { name = _; nat; expr = inner } ->
    let struct_type = infer ctx inner |> whnf ctx in
    let const, ty_args = Expr.get_apps struct_type in
    (match const |> Expr.node with
    | Const { name; uparams } ->
      let inductive_info = Env.find (Ctx.env ctx) name in
      let ctor_names = Decl.get_inductive_ctors inductive_info in
      let ctor_num_params = Decl.get_inductive_num_params inductive_info in
      assert (CCList.length ctor_names = 1);
      let ctor_info = ctor_names |> CCList.hd |> Env.find (Ctx.env ctx) in
      let ctor_info_type = ctor_info |> Decl.get_type in
      let ctor_uparams = CCList.map Level.param (Decl.get_uparams ctor_info) in
      let ctor_type =
        ref (Expr.subst_levels ctor_info_type ctor_uparams uparams)
      in
      let ty_param_args = CCList.take ctor_num_params ty_args in
      for _j = 0 to CCList.length ty_param_args - 1 do
        let for_ty = whnf ctx !ctor_type in
        match for_ty |> Expr.node with
        | Forall { body; _ } ->
          let ty_arg = CCList.nth ty_param_args _j in
          ctor_type := Expr.instantiate ~free_var:ty_arg ~expr:body
        | _ ->
          L.err "infer Proj: ctor type instantiation expected Forall, got %a"
            (TypeError "infer Proj: ctor type not a forall")
            Expr.pp for_ty
      done;
      for i = 0 to nat - 1 do
        let for_ty = whnf ctx !ctor_type in
        match for_ty |> Expr.node with
        | Forall { body; _ } ->
          let proj_expr = Expr.proj name i inner in
          ctor_type := Expr.instantiate ~free_var:proj_expr ~expr:body
        | _ ->
          L.err "infer Proj: projection instantiation expected Forall, got %a"
            (TypeError "infer Proj: projection type not a forall")
            Expr.pp for_ty
      done;
      let final_ty = whnf ctx !ctor_type in
      (match final_ty |> Expr.node with
      | Forall { btype; _ } -> btype
      | _ ->
        L.err "infer Proj: final type expected Forall, got %a"
          (TypeError "infer Proj: final type not a forall")
          Expr.pp final_ty)
    | _ ->
      L.err "infer Proj: expected a const, got @[%a@] for @[%a@]"
        (TypeError "infer Proj: struct type not a const")
        Expr.pp struct_type Expr.pp expr)
  | Literal lit ->
    (match lit with
    | Expr.NatLit _ -> Expr.const (Name.of_string "Nat")
    | Expr.StrLit _ -> Expr.const (Name.of_string "String"))
  | BoundVar _ ->
    L.err
      "@[<v 0>@[infer BoundVar: encountered bound variable during inference:@,\
       @[<hv 2> %a@]@]@]"
      (TypeError "infer: unexpected bound variable")
      Expr.pp expr

and infer_sort_of ctx (expr : Expr.t) =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  match whnf ctx (infer ctx expr) |> Expr.node with
  | Sort lvl -> lvl
  | _ ->
    L.err "infer_sort_of: expr %a is not a sort"
      (TypeError "infer_sort_of: not a sort")
      Expr.pp (infer ctx expr)

and whnf (ctx : Ctx.t) (expr : Expr.t) : Expr.t =
  with_trace
    ~enter:(fun input -> Ctx.Whnf_trace.enter ctx.whnf_trace ctx.logger input)
    ~leave_success:(fun frame out ->
      Ctx.Whnf_trace.leave_success ctx.whnf_trace ctx.logger frame out)
    ~leave_failure:(fun frame exn ->
      Ctx.Whnf_trace.leave_failure ctx.whnf_trace ctx.logger frame exn)
    ~memo:ctx.whnf_memo ~key:(Expr.tag expr) expr (fun () -> whnf_impl ctx expr)

and whnf_impl (ctx : Ctx.t) (expr : Expr.t) : Expr.t =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  match expr |> Expr.node with
  | Expr.Sort u -> Expr.sort (Level.simplify u)
  | Expr.App (_, _) ->
    let hd, args = Expr.get_apps expr in
    (match Reduce.nat_lit_reduce ctx expr ~whnf with
    | Some r ->
      L.debug "whnf: nat-builtin";
      whnf ctx r
    | None ->
      if Reduce.is_nat_builtin expr then
        expr
      else (
        let hd' =
          match hd |> Expr.node with
          | Expr.Const _ -> Reduce.delta_at_head ctx hd
          | _ -> whnf ctx hd
        in
        let e1 = Expr.mk_app hd' args in
        let e2 = Reduce.beta e1 in
        let e3 = Reduce.iota_at_head ctx e2 ~whnf |> Reduce.beta in
        if e3 == expr then
          e3
        else (
          if hd' != hd then L.debug "whnf App: delta";
          if e2 != e1 then L.debug "whnf App: beta";
          if e3 != e2 then L.debug "whnf App: iota";
          whnf ctx e3
        )
      ))
  | Expr.Let { value; body; _ } ->
    L.debug "whnf: zeta";
    whnf ctx (Expr.instantiate ~free_var:value ~expr:body)
  | Expr.Const { name; _ } ->
    (* Nat.zero is definitionally equal to NatLit 0 in the Lean kernel.
       Normalise here so that is_def_eq doesn't need a Const-vs-Literal
       case. *)
    if name = Name.Str (Name.Str (Name.Anon, "Nat"), "zero") then
      Expr.natlit Z.zero
    else if Reduce.is_nat_builtin_name name then
      expr
    else (
      let e' = Reduce.delta_at_head ctx expr in
      if e' == expr then
        expr
      else (
        L.debug "whnf Const: delta";
        whnf ctx e'
      )
    )
  | Expr.Forall _ -> expr
  | Expr.Proj { name; nat; expr = inner } ->
    let inner' = whnf ctx inner in
    let inner' =
      match Expr.node inner' with
      | Expr.Literal (Expr.StrLit s) -> Reduce.string_lit_to_ctor s
      | _ -> inner'
    in
    let hd, args = Expr.get_apps inner' in
    (match Expr.node hd with
    | Expr.Const { name = cname; _ } ->
      let ind_decl = Env.find (Ctx.env ctx) name in
      let ctor_names = Decl.get_inductive_ctors ind_decl in
      if List.mem cname ctor_names then (
        let num_params = Decl.get_inductive_num_params ind_decl in
        let idx = nat + num_params in
        match CCList.get_at_idx idx args with
        | Some field ->
          L.debug "whnf: proj-reduce (#%d)" nat;
          whnf ctx field
        | None -> Expr.proj name nat inner'
      ) else
        Expr.proj name nat inner'
    | _ -> Expr.proj name nat inner')
  | _ -> expr

and is_def_eq ctx e1 e2 =
  let frame = Ctx.Defeq_trace.enter ctx.defeq_trace ctx.logger (e1, e2) in
  match is_def_eq_impl ctx e1 e2 with
  | ans ->
    Ctx.Defeq_trace.leave_success ctx.defeq_trace ctx.logger frame ans;
    ans
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    Ctx.Defeq_trace.leave_failure ctx.defeq_trace ctx.logger frame exn;
    Printexc.raise_with_backtrace exn bt

and is_def_eq_impl ctx e1 e2 =
  let module L = (val Ctx.logger ctx : Ctx.LOGGER) in
  if e1 == e2 then
    true
  else (
    (* Early proof-irrelevance check, before whnf. *)
    let is_prop ty =
      match Expr.node (whnf ctx (infer ctx ty)) with
      | Expr.Sort u -> Level.is_zero u
      | _ -> false
    in
    let s = infer ctx e1 in
    let t = infer ctx e2 in
    if is_prop s && is_prop t && is_def_eq ctx s t then
      true
    else (
      let e1' = whnf ctx e1 in
      let e2' = whnf ctx e2 in
      if e1' == e2' then
        true
      else (
        let result =
          match Expr.node e1', Expr.node e2' with
          | Expr.Sort u1, Expr.Sort u2 ->
            if Level.(u1 === u2) then
              true
            else (
              L.debug "defeq: Sort level mismatch: %a vs %a" Level.pp u1
                Level.pp u2;
              false
            )
          | Expr.FreeVar { fvarId = f1; _ }, Expr.FreeVar { fvarId = f2; _ } ->
            if f1 = f2 then
              true
            else (
              L.debug "defeq: FreeVar id mismatch";
              false
            )
          | ( Expr.Forall { name = n; btype = s; body = a; binfo },
              Expr.Forall { btype = t; body = b; _ } ) ->
            if is_def_eq ctx s t then (
              let free_var = Ctx.fresh_fvar ctx ~name:n ~btype:s ~binfo in
              let r =
                is_def_eq ctx
                  (Expr.instantiate ~free_var ~expr:a)
                  (Expr.instantiate ~free_var ~expr:b)
              in
              if not r then L.debug "defeq: Forall body mismatch";
              r
            ) else (
              L.debug "defeq: Forall btype mismatch";
              false
            )
          | Expr.App (f, a), Expr.App (g, b) ->
            let fn_eq =
              is_def_eq ctx
                (Reduce.delta_at_head ctx f)
                (Reduce.delta_at_head ctx g)
            in
            if not fn_eq then (
              L.debug "defeq: App fn mismatch";
              false
            ) else (
              let arg_eq = is_def_eq ctx a b in
              if not arg_eq then (
                L.debug "defeq: App arg mismatch";
                false
              ) else
                true
            )
          | ( Expr.Const { name = n1; uparams = us },
              Expr.Const { name = n2; uparams = vs } ) ->
            if
              n1 = n2
              && CCList.fold_left2
                   (fun acc u v -> acc && Level.(u === v))
                   true us vs
            then
              true
            else (
              L.debug "defeq: Const mismatch: %a vs %a" Name.pp n1 Name.pp n2;
              false
            )
          | ( Expr.Lam { name = n; btype = s; body = a; binfo },
              Expr.Lam { btype = t; body = b; _ } ) ->
            if is_def_eq ctx s t then (
              let free_var = Ctx.fresh_fvar ctx ~name:n ~btype:s ~binfo in
              let r =
                is_def_eq ctx
                  (Expr.instantiate ~free_var ~expr:a)
                  (Expr.instantiate ~free_var ~expr:b)
              in
              if not r then L.debug "defeq: Lam body mismatch";
              r
            ) else (
              L.debug "defeq: Lam btype mismatch";
              false
            )
          | Literal (Expr.NatLit n1), Literal (Expr.NatLit n2) ->
            if Z.equal n1 n2 then
              true
            else (
              L.debug "defeq: NatLit mismatch: %s vs %s" (Z.to_string n1)
                (Z.to_string n2);
              false
            )
          | Proj { nat = n1; expr = e1; _ }, Proj { nat = n2; expr = e2; _ } ->
            if n1 == n2 && is_def_eq ctx e1 e2 then
              true
            else (
              L.debug "defeq: Proj mismatch";
              false
            )
          | ( Expr.Let { name = n1; btype = s1; value = v1; body = a },
              Expr.Let { btype = s2; value = v2; body = b; _ } ) ->
            if is_def_eq ctx s1 s2 && is_def_eq ctx v1 v2 then (
              let free_var =
                Ctx.fresh_fvar ctx ~name:n1 ~btype:s1 ~binfo:Expr.Default
              in
              let r =
                is_def_eq ctx
                  (Expr.instantiate ~free_var ~expr:a)
                  (Expr.instantiate ~free_var ~expr:b)
              in
              if not r then L.debug "defeq: Let body mismatch";
              r
            ) else (
              L.debug "defeq: Let btype/value mismatch";
              false
            )
          | _ ->
            let try_struct_eta x y =
              let hd, y_args = Expr.get_apps y in
              match Expr.node hd with
              | Expr.Const { name = ctor_name; _ } ->
                (match Env.find_opt (Ctx.env ctx) ctor_name with
                | Some (Decl.Ctor { inductive_name; num_params; num_fields; _ })
                  ->
                  (match Env.find_opt (Ctx.env ctx) inductive_name with
                  | Some
                      (Decl.Inductive
                        { ctor_names; num_idx; is_recursive; _ })
                    when List.length ctor_names = 1
                         && num_idx = 0
                         && (not is_recursive)
                         && List.length y_args = num_params + num_fields ->
                    if not (is_def_eq ctx (infer ctx x) (infer ctx y)) then
                      false
                    else (
                      L.debug "defeq: struct-eta for %a" Name.pp inductive_name;
                      let rec check i =
                        if i >= num_fields then
                          true
                        else
                          is_def_eq ctx
                            (Expr.proj inductive_name i x)
                            (List.nth y_args (num_params + i))
                          && check (i + 1)
                      in
                      check 0
                    )
                  | _ -> false)
                | _ -> false)
              | _ -> false
            in
            if try_struct_eta e1' e2' || try_struct_eta e2' e1' then
              true
            else (
              let try_lam_eta lam other =
                match Expr.node lam with
                | Expr.Lam { name; btype; body; binfo } ->
                  let fv = Ctx.fresh_fvar ctx ~name ~btype ~binfo in
                  is_def_eq ctx
                    (Expr.instantiate ~free_var:fv ~expr:body)
                    (Expr.app other fv)
                | _ -> false
              in
              if try_lam_eta e1' e2' || try_lam_eta e2' e1' then
                true
              else
                L.err
                  "@[<v 0>[isDefEq] structural mismatch@,\
                   \ lhs = %a@,\
                   \ rhs = %a@]"
                  (Defeq_failure "isDefEq: structural mismatch")
                  Expr.pp e1 Expr.pp e2
            )
        in
        result
      )
    )
  )
