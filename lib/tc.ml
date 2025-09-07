[@@@warning "-27"]

exception TypeError of Expr.t

exception Not_well_posed

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Checker"
end)

let rec infer (env : Env.t) (expr : Expr.t) : Expr.t =
  match (expr : Expr.t) with
  | Expr.Sort u -> Expr.Sort (Level.Succ u)
  | Expr.FreeVar { name; expr; info; fvarId } ->
    (*
       infer FVar id binder:
       binder.type
    *)
    expr
  | Expr.Lam { name; btype; binfo; body } ->
    (*
      infer Lambda(binder, body):
       assert! infersAsSort(binder.type)
       let binderFvar := fvar(binder)
       let bodyType := infer $ instantiate(body, binderFVar)
       Pi binder (abstract bodyType binderFVar)
    *)
    (match infer env btype with
    | Expr.Sort _ ->
      let binder_free_var =
        Expr.FreeVar
          {
            name;
            expr = btype;
            info = binfo;
            fvarId = Nyaya_parser.Util.Uid.mk ();
          }
      in
      let body_type =
        infer env (Expr.instantiate ~free_var:binder_free_var ~expr:body)
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
  | Expr.Forall { name; btype; binfo; body } ->
    (*
      infer Pi binder body:
      let l := inferSortOf binder
      let r := inferSortOf $ instantiate body (fvar(binder))
      imax(l, r)
    *)
    let l = infer_sort_of env btype in
    let free_var =
      Expr.FreeVar
        {
          name;
          expr = btype;
          info = binfo;
          fvarId = Nyaya_parser.Util.Uid.mk ();
        }
    in
    let r = infer_sort_of env (Expr.instantiate ~free_var ~expr:body) in
    Expr.sort (Level.IMax (l, r) |> Level.simplify)
  | Expr.Const { name; uparams } ->
    (*
       infer Const name levels:
       let knownType := environment[name].type
       substituteLevels (e := knownType) (ks := knownType.uparams) (vs := levels)
    *)
    let known_type : Decl.t = Hashtbl.find env name in
    let known_type_uparams =
      CCList.map Level.param (Decl.get_uparams known_type)
    in
    Logger.debug "known_type : %a" Decl.pp known_type;
    Logger.debug "known_type_uparams : %a" (CCList.pp Level.pp)
      known_type_uparams;
    Logger.debug "uparams : %a" (CCList.pp Level.pp) uparams;
    Logger.debug "Result : %a" Expr.pp
      (Expr.subst_levels
         (known_type |> Decl.get_type)
         known_type_uparams uparams);
    Expr.subst_levels (known_type |> Decl.get_type) known_type_uparams uparams
  | _ -> Logger.err "failed inferring: %a" (TypeError expr) Expr.pp expr

and infer_sort_of env (expr : Expr.t) =
  match whnf (infer env expr) with
  | Sort lvl -> lvl
  | _ ->
    Logger.err "infer_sort_of: expr %a is not a sort" (TypeError expr) Expr.pp
      expr

and whnf (expr : Expr.t) : Expr.t =
  match expr with
  | Expr.Sort u -> Expr.Sort (Level.simplify u)
  | _ -> Logger.err "failed reducing: %a" (TypeError expr) Expr.pp expr

and isDefEq e1 e2 =
  Logger.debug "checking def eq : %a ===?=== %a" Expr.pp e1 Expr.pp e2;
  match e1, e2 with
  | Expr.Sort u1, Expr.Sort u2 -> Level.(u1 === u2)
  | Expr.FreeVar { fvarId = f1; _ }, Expr.FreeVar { fvarId = f2; _ } -> f1 = f2
  | ( Expr.Forall { name = n; btype = s; body = a; binfo },
      Expr.Forall { btype = t; body = b; _ } ) ->
    if isDefEq s t then (
      let free_var =
        Expr.FreeVar
          {
            name = n;
            expr = s;
            info = binfo;
            fvarId = Nyaya_parser.Util.Uid.mk ();
          }
      in
      isDefEq
        (Expr.instantiate ~free_var ~expr:a)
        (Expr.instantiate ~free_var ~expr:b)
    ) else
      false
  | _ ->
    Logger.err "failed def eq: %a =?= %a" (TypeError e1) Expr.pp e1 Expr.pp e2

let check (env : Env.t) (decl : Decl.t) : bool =
  match (decl : Decl.t) with
  | Def { info; value; red_hint = _red_hint } ->
    Logger.debug "Checking value %a against %a" Expr.pp value Expr.pp info.ty;
    isDefEq (infer env value) info.ty
  | Axiom _ -> true
  | _ ->
    Logger.err "failed checking decl: %a" (Failure "type checking failed")
      Decl.pp decl

let well_posed (env : Env.t) (info : Decl.decl_info) : bool =
  let rec dup_exist = function
    | [] -> false
    | hd :: tl -> List.exists (( = ) hd) tl || dup_exist tl
  in
  let no_dup_uparams = dup_exist info.uparams |> not in
  let no_free_vars = Expr.has_free_vars info.ty |> not in
  let type_is_sort =
    try
      match infer env info.ty with
      | Expr.Sort _ ->
        Logger.debug "info.name info.ty : %a %a@." Name.pp info.name Expr.pp
          (infer env info.ty);
        true
      | _ ->
        Logger.debug "info.ty : %a@." Expr.pp (infer env info.ty);
        false
    with TypeError e ->
      Logger.err "failed inferring: %a" (TypeError e) Expr.pp e
  in

  Logger.debug "(no_dup_uparams,no_free_vars,type_is_sort) = (%s,%s,%s)"
    (string_of_bool no_dup_uparams)
    (string_of_bool no_free_vars)
    (string_of_bool type_is_sort);
  no_dup_uparams && no_free_vars && type_is_sort

(** Check well-posedness of all declarations in the environment *)
let check_all_well_posed (env : Env.t) : bool =
  Hashtbl.fold
    (fun name decl acc ->
      let info = Decl.get_decl_info decl in
      let is_well_posed = well_posed env info in
      if not is_well_posed then
        Logger.err "Declaration %a is not well-posed" Not_well_posed Name.pp
          name;
      acc && is_well_posed)
    env true

let typecheck (env : Env.t) =
  let all_well_posed = check_all_well_posed env in
  Logger.debug "All declarations well-posed: %b@." all_well_posed;
  let iter = env |> Iter.of_hashtbl in
  let success = ref 0 in
  Iter.iter2
    (fun n d ->
      try
        if check env d then
          success := !success + 1
        else
          ()
      with TypeError e ->
        Logger.err "Type checking failed when checking %a." (TypeError e)
          Name.pp n)
    iter;
  Logger.success "Successfully checked %d declarations in environment." !success
