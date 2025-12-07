(* TODO: Refactor this whole module to replace manual match/error with Result.bind + projection helper. *)

[@@@warning "-27"]

exception TypeError of Expr.t

exception Not_well_posed

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Checker"
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
    Logger.debug "@[Beta reducing @[<hov 2> %a@]]" pp e;
    let rec aux f args =
      match f, args with
      | Lam { body; _ }, v :: vs ->
        aux (instantiate ~free_var:body ~expr:v ()) vs
      | _, _ -> mk_app f args
    in
    let f, args = get_apps e in
    let ans = aux f args in
    Logger.debugf
      (fun fpf (t1, t2) ->
        CCFormat.fprintf fpf
          "@[<hov 0>After beta reduction@;\
           @[<hov 2>%a@]@;\
           becomes@;\
           @[<hov 2>%a@]@]" pp t1 pp t2)
      (e, ans);
    ans

  let delta_at_head (env : Env.t) f =
    let module Logger = (val env.logger) in
    (* One-step delta reduction of the head. *)
    match f with
    | Const { name; uparams } as c ->
      let decl = Hashtbl.find env.tbl name in
      let decl_value = decl |> Decl.get_value in
      let ans =
        (* TODO: add a note about this in the notebook. *)
        match decl_value with
        | Some v ->
          let decl_uparams =
            CCList.map Level.param (decl |> Decl.get_uparams)
          in
          Expr.subst_levels v decl_uparams uparams
        | None -> c
      in
      Logger.debugf
        (fun fpf (t1, t2) ->
          CCFormat.fprintf fpf
            "@[<v 0>After delta reduction@,\
             @[<hv 2>%a@]@,\
             becomes@,\
             @[<hv 2>%a@]@]" Expr.pp t1 Expr.pp t2)
        (f, ans);
      ans
    | _ -> f
end

(** Infer the type of the given [expr]. TODO: this needs some aggressive optimization in the form of memoization. *)
let rec infer (env : Env.t) (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  Logger.debugf Pp.pp_inferring expr;
  match (expr : Expr.t) with
  | Expr.Sort u -> Expr.Sort (Level.Succ u)
  | Expr.FreeVar { name; expr; info; fvarId } ->
    (*
       infer FVar id binder:
       binder.type
    *)
    Logger.debugf
      (fun fpf e ->
        CCFormat.fprintf fpf
          "@[@[%a@]@, is a free var of known type@, @[<hov 2> %a@]@]" Name.pp
          name Expr.pp e)
      expr;
    expr
  | Expr.Lam { name; btype; binfo; body } ->
    (*
      infer Lambda(binder, body):
       assert! infersAsSort(binder.type)
       let binderFvar := fvar(binder)
       let bodyType := infer $ instantiate(body, binderFVar)
       Pi binder (abstract bodyType binderFVar)
    *)
    (match whnf env (infer env btype) with
    | Expr.Sort _ ->
      Logger.debugf
        (fun fpf e ->
          CCFormat.fprintf fpf "@[binder type %a is a sort@]" Expr.pp e)
        btype;
      let binder_free_var =
        Expr.FreeVar
          {
            name;
            expr = btype;
            info = binfo;
            fvarId = Nyaya_parser.Util.Uid.mk ();
          }
      in
      Logger.debugf
        (fun fpf (e1, e2) ->
          CCFormat.fprintf fpf
            "@[<hov 0>Created a free variable of type @[<hov 2>%a@] as @[<hov \
             2>%a@]@]"
            Expr.pp e1 Expr.pp e2)
        (btype, binder_free_var);
      let body_type =
        infer env
          (Expr.instantiate ~logger:env.logger ~free_var:binder_free_var
             ~expr:body ())
      in
      let target_id = Expr.get_fvar_id binder_free_var in
      Expr.Forall
        {
          name;
          btype;
          binfo;
          body = Expr.abstract_fvar ~target_id ~k:0 body_type;
        }
    | _ ->
      Logger.err "binder type is not a sort: %a" (TypeError expr) Expr.pp expr)
  | Expr.Forall { name; btype; binfo; body } as _e ->
    (*
      infer Pi binder body:
      let l := inferSortOf binder
      let r := inferSortOf $ instantiate body (fvar(binder))
      imax(l, r)
    *)
    let l = infer_sort_of env btype in
    Logger.debug "@[Level of %a is %a@]" Expr.pp btype Level.pp l;
    let free_var =
      Expr.FreeVar
        {
          name;
          expr = btype;
          info = binfo;
          fvarId = Nyaya_parser.Util.Uid.mk ();
        }
    in
    Logger.debugf
      (fun fpf (e1, e2) ->
        CCFormat.fprintf fpf
          "@[<hov 0>Created a free variable of type @[<hov 2>%a@] as @[<hov \
           2>%a@]@]"
          Expr.pp e1 Expr.pp e2)
      (btype, free_var);
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
    Logger.debugf
      (fun fpf t ->
        CCFormat.fprintf fpf "@[Now inferring constant of name %a@]" Name.pp t)
      name;
    let known_type : Decl.t = Hashtbl.find env.tbl name in
    let known_type_uparams =
      CCList.map Level.param (Decl.get_uparams known_type)
    in
    let res =
      Expr.subst_levels (known_type |> Decl.get_type) known_type_uparams uparams
    in
    Logger.debug "Inferred constant type : %a" Expr.pp res;
    res
  | Expr.App (f, arg) as e ->
    (*
    infer App(f, arg):
      match (whnf $ infer f) with
      | Pi binder body => 
        assert! defEq(binder.type, infer arg)
        instantiate(body, arg)
      | _ => error
    *)
    (match whnf env (infer env f) with
    | Expr.Forall { btype; body; _ } ->
      Logger.debugf Pp.pp_defeq (btype, infer env arg);
      if not (isDefEq env btype (infer env arg)) then
        Logger.err
          "@[Defeq check failed in expr = %a between @,\
           btype = %a and@,\
          \ inferred arg = %a@]" (Failure "failed 1") Expr.pp e Expr.pp btype
          Expr.pp (infer env arg);
      let p = Expr.instantiate ~logger:env.logger ~free_var:arg ~expr:body () in
      Logger.debug "Inferred type of %a to be %a" Expr.pp e Expr.pp p;
      p
    | e ->
      Logger.err "Failed infer at app, got @[%a@] instead of a forall"
        (TypeError f) Expr.pp e)
  | Let { name; btype; value; body } as e ->
    Logger.debug "Inferring Let : %a" Expr.pp e;
    (*
       infer Let binder val body:
       assert! inferSortOf binder
       assert! defEq(infer(val), binder.type)
       infer (instantiate body val)
    *)
    (match infer env btype with
    | Sort _ ->
      Logger.debug "Inferring Let : %a" Expr.pp e;
      if not (isDefEq env btype (infer env value)) then
        Logger.err "@[btype = %a @. arg = %a@]" (Failure "failed 2") Expr.pp
          btype Expr.pp (infer env value);
      infer env
        (Expr.instantiate ~logger:env.logger ~free_var:value ~expr:body ())
    | _ -> Logger.err "binder type is not a sort: %a" (TypeError expr) Expr.pp e)
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
    (match const with
    | Const { name; uparams } ->
      Logger.info "const : %a" Expr.pp const;
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
      Logger.info "ctor_names : %a@." (CCList.pp Name.pp) ctor_names;
      Logger.info "ctor_type : %a" Expr.pp !ctor_type;
      let ty_param_args = CCList.take ctor_num_params ty_args in
      for i = 0 to CCList.length ty_param_args - 1 do
        let for_ty = whnf env !ctor_type in
        match for_ty with
        | Forall { body; _ } ->
          let ty_arg = CCList.nth ty_param_args i in
          ctor_type :=
            Expr.instantiate ~logger:env.logger ~free_var:ty_arg ~expr:body ()
        | _ ->
          Logger.err
            "Constructor type instantiation failed: expected Forall type but \
             got %a"
            (TypeError for_ty) Expr.pp for_ty
      done;
      (* Now, instantiate the projections *)
      for i = 0 to nat - 1 do
        let for_ty = whnf env !ctor_type in
        match for_ty with
        | Forall { body; _ } ->
          let proj_expr = Expr.Proj { name; nat = i; expr } in
          ctor_type :=
            Expr.instantiate ~logger:env.logger ~free_var:proj_expr ~expr:body
              ()
        | _ ->
          Logger.err
            "Projection type instantiation failed: expected Forall type but \
             got %a"
            (TypeError for_ty) Expr.pp for_ty
      done;
      (* Now, the next binder's type is the projection type *)
      let final_ty = whnf env !ctor_type in
      (match final_ty with
      | Forall { btype; _ } -> btype
      | _ ->
        Logger.err "Final type error: expected Forall type but got %a"
          (TypeError final_ty) Expr.pp final_ty)
    | e ->
      Logger.err
        "@[While inferring @[%a@] expected a const, got @[%a@] instead@]"
        (TypeError e) Expr.pp struct_type Expr.pp e)
  | Literal lit ->
    (match lit with
    | Expr.NatLit _ -> Expr.const (Name.of_string "Nat")
    | Expr.StrLit _ -> Expr.const (Name.of_string "String"))
  | BoundVar _ as expr ->
    (* Since we are using the locally nameless approach, we should not run into
       bound variables during type inference, because all open binders will be
       instantiated with the appropriate free variables. *)
    Logger.err
      "@[<v 0>@[Fatal error: encountered bound variable during inference:@,\
       @[<hv 2> %a@]@]@]" (TypeError expr) Expr.pp expr

and infer_sort_of env (expr : Expr.t) =
  let module Logger = (val env.logger) in
  match whnf env (infer env expr) with
  | Sort lvl -> lvl
  | _ ->
    Logger.err "infer_sort_of: expr %a is not a sort" (TypeError expr) Expr.pp
      (infer env expr)

and whnf (env : Env.t) (expr : Expr.t) : Expr.t =
  let module Logger = (val env.logger) in
  match expr with
  | Expr.Sort u -> Expr.Sort (Level.simplify u)
  | Expr.App (f, arg) ->
    (* TODO: do we need to whnf the arg? *)
    let f' = Reduce.delta_at_head env f in
    let arg' = whnf env arg in
    (* Beta reduction*)
    Reduce.beta (App (f', arg'))
  | Expr.Let { name; btype; value; body } ->
    (* Zeta reduction*)
    Expr.instantiate ~logger:env.logger ~free_var:value ~expr:body ()
  | Expr.Const { name; uparams } as e ->
    (* Delta reduction *)
    Reduce.delta_at_head env e
  | Expr.Forall { name; btype; binfo; body } ->
    (* Reduce the domain type *)
    Logger.debug "Reducing Foralls";
    Expr.Forall { name; btype = whnf env btype; binfo; body }
  | e ->
    Logger.debug "not reducing: %a" Expr.pp expr;
    e

(* TODO: optimize def eq checking by implementing union-find. *)
and isDefEq env e1 e2 =
  let module Logger = (val env.logger) in
  Logger.debugf Pp.pp_defeq (e1, e2);
  match e1, e2 with
  | Expr.Sort u1, Expr.Sort u2 -> Level.(u1 === u2)
  | Expr.FreeVar { fvarId = f1; _ }, Expr.FreeVar { fvarId = f2; _ } -> f1 = f2
  | ( Expr.Forall { name = n; btype = s; body = a; binfo },
      Expr.Forall { btype = t; body = b; _ } ) ->
    if isDefEq env s t then (
      let free_var =
        Expr.FreeVar
          {
            name = n;
            expr = s;
            info = binfo;
            fvarId = Nyaya_parser.Util.Uid.mk ();
          }
      in
      isDefEq env
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ())
    ) else
      false
  | Expr.App (f, a), Expr.App (g, b) ->
    isDefEq env (Reduce.delta_at_head env f) (Reduce.delta_at_head env g)
    && isDefEq env a b
  | ( Expr.Const { name = n1; uparams = us },
      Expr.Const { name = n2; uparams = vs } ) ->
    (* We test for structural equality of names here and not pointer equality
       since there are names which are not pointer equal (e.g., we infer Nat
       literals as Const("Nat",[]))
    *)
    n1 = n2
    && CCList.fold_left2 (fun acc u v -> acc && Level.(u === v)) true us vs
  | ( Expr.Lam { name = n; btype = s; body = a; binfo },
      Expr.Lam { btype = t; body = b; _ } ) ->
    if isDefEq env s t then (
      let free_var =
        Expr.FreeVar
          {
            name = n;
            expr = s;
            info = binfo;
            fvarId = Nyaya_parser.Util.Uid.mk ();
          }
      in
      isDefEq env
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ())
    ) else
      false
  | Literal (Expr.NatLit n1), Literal (Expr.NatLit n2) -> Z.equal n1 n2
  | Proj { nat = n1; expr = e1; _ }, Proj { nat = n2; expr = e2; _ } ->
    n1 == n2 && isDefEq env e1 e2
  | ( Expr.Let { name = n1; btype = s1; value = v1; body = a },
      Expr.Let { name = n2; btype = s2; value = v2; body = b } ) ->
    if isDefEq env s1 s2 && isDefEq env v1 v2 then (
      let free_var =
        Expr.FreeVar
          {
            name = n1;
            expr = s1;
            info = Expr.Default;
            fvarId = Nyaya_parser.Util.Uid.mk ();
          }
      in
      isDefEq env
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:a ())
        (Expr.instantiate ~logger:env.logger ~free_var ~expr:b ())
    ) else
      false
  | _ ->
    Logger.err "failed def eq: %a =?= %a" (TypeError e1) Expr.pp e1 Expr.pp e2

let check (env : Env.t) (decl : Decl.t) : bool =
  let module Logger = (val env.logger) in
  Logger.info "@[Now type-checking %a.@]" Decl.pp decl;
  match (decl : Decl.t) with
  | Def { info; value; red_hint = _red_hint } ->
    (* Logger.debug "@[<v 2>@.Checking value @,@[<2>%a@] against @,@[<2>%a@]@]"
       Expr.pp value Expr.pp info.ty; *)
    Logger.debugf Pp.pp_check (value, info.ty);
    let ans = isDefEq env (infer env value) info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Thm { info; value } ->
    (* Logger.debug "@[<v 2>@.Checking value @,@[<2>%a@] against @,@[<2>%a@]@]"
       Expr.pp value Expr.pp info.ty; *)
    Logger.debugf Pp.pp_check (value, info.ty);
    let ans = isDefEq env (infer env value) info.ty in
    Logger.success "@[Successfully type-checked @[%a@].@]" Name.pp info.name;
    ans
  | Axiom { name; uparams; ty } ->
    Logger.debugf Pp.pp_check_name (name, ty);
    true
  | _ ->
    Logger.warn "not checking decl: %a" Decl.pp decl;
    true
(* Logger.err "failed checking decl: %a" (Failure "type checking failed")
   Decl.pp decl *)

(** We check if any declaration in the environment has 1) duplicate uparams or 2) lingering free variables in the type or 3) the type of its type is a sort. 
  If yes, we call that declaration well-posed and only typecheck those. *)
let well_posed (env : Env.t) (info : Decl.decl_info) : bool =
  let module Logger = (val env.logger) in
  Logger.info "Checking if %a is well-posed." Name.pp info.name;
  let rec dup_exist = function
    | [] -> false
    | hd :: tl -> List.exists (( = ) hd) tl || dup_exist tl
  in
  let no_dup_uparams = dup_exist info.uparams |> not in
  let no_free_vars = Expr.has_free_vars info.ty |> not in
  let type_is_sort =
    Logger.info "Checking if type %a is sort" Expr.pp info.ty;
    try
      match infer env info.ty with
      | Expr.Sort _ ->
        Logger.info "Type of %a is sort" Name.pp info.name;
        true
      | _ ->
        Logger.debug "info.ty : %a@." Expr.pp (infer env info.ty);
        false
    with TypeError e ->
      Logger.err "While inferring %a could not infer type: %a" (TypeError e)
        Name.pp info.name Pp.pp_failed_inferring e
  in

  Logger.debug "(no_dup_uparams,no_free_vars,type_is_sort) = (%s,%s,%s)"
    (string_of_bool no_dup_uparams)
    (string_of_bool no_free_vars)
    (string_of_bool type_is_sort);
  no_dup_uparams && no_free_vars && type_is_sort

let typecheck (env : Env.t) =
  let iter = env.tbl |> Iter.of_hashtbl in
  let success = ref 0 in
  Iter.iter2
    (fun n d ->
      let module DeclLogger : Env.LOGGER = Nyaya_parser.Util.MakeLogger (struct
        let header = CCFormat.to_string Name.pp n
      end) in
      let env = Env.with_logger env (module DeclLogger) in
      let info = Decl.get_decl_info d in
      (* Check well-posedness. *)
      let is_well_posed = well_posed env info in
      if not is_well_posed then
        DeclLogger.err "Declaration %a is not well-posed" Not_well_posed Name.pp
          n
      else
        DeclLogger.info "Declaration %a is well-posed." Name.pp n;
      (* Decl is well-posed, so perform typechecking. *)
      try
        if check env d then
          success := !success + 1
        else
          ()
      with TypeError e ->
        Logger.success "Failed after checking %d declarations in environment."
          !success;
        Logger.err "Type checking failed when checking %a." (TypeError e)
          Name.pp n)
    iter;
  Logger.success "Successfully checked %d declarations in environment." !success
