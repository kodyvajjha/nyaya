exception TypeError of Expr.t

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Checker"
end)

let infer (_env : Env.t) (expr : Expr.t) : Expr.t =
  match (expr : Expr.t) with
  | Expr.Sort u -> Expr.Sort (Level.Succ u)
  | _ -> Logger.err "failed inferring: %a" (TypeError expr) Expr.pp expr

(* Helper to extract decl_info from any declaration *)
let get_decl_info (decl : Decl.t) : Decl.decl_info =
  match decl with
  | Decl.Axiom info -> info
  | Decl.Def { info; _ } -> info
  | Decl.Thm { info; _ } -> info
  | Decl.Opaque { info; _ } -> info
  | Decl.Quot { info } -> info
  | Decl.Inductive { info; _ } -> info
  | Decl.Ctor { info; _ } -> info
  | Decl.Rec { info; _ } -> info

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
      | Expr.Sort _ -> true
      | _ -> false
    with TypeError _ -> false
  in
  no_dup_uparams && no_free_vars && type_is_sort

(* Check well-posedness of all declarations in the environment *)
let check_all_well_posed (env : Env.t) : bool =
  Hashtbl.fold
    (fun name decl acc ->
      let info = get_decl_info decl in
      let is_well_posed = well_posed env info in
      if not is_well_posed then
        Logger.info "Declaration %a is not well-posed" Name.pp name;
      acc && is_well_posed)
    env true

let whnf (expr : Expr.t) : Expr.t =
  match expr with
  | Expr.Sort u -> Expr.Sort (Level.simplify u)
  | _ -> Logger.err "failed reducing: %a" (TypeError expr) Expr.pp expr

let isDefEq e1 e2 =
  match e1, e2 with
  | Expr.Sort u1, Expr.Sort u2 -> Level.(u1 === u2)
  | _ ->
    Logger.err "failed def eq: %a =?= %a" (TypeError e1) Expr.pp e1 Expr.pp e2

let check (_env : Env.t) (decl : Decl.t) : bool =
  match (decl : Decl.t) with
  | Def { info; value; red_hint = _red_hint } ->
    isDefEq (infer _env value) info.ty
  | _ ->
    Logger.err "failed checking decl: %a" (Failure "type checking failed")
      Decl.pp decl

let typecheck (env : Env.t) =
  let iter = env |> Iter.of_hashtbl in
  Iter.iter2
    (fun n d ->
      try check env d |> string_of_bool |> print_endline
      with TypeError _ ->
        Logger.info "Type checking failed when checking %a" Name.pp n)
    iter
