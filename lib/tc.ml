exception TypeError of Expr.t

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Checker"
end)

let infer (_env : Env.t) (expr : Expr.t) : Expr.t =
  match (expr : Expr.t) with
  | Expr.Sort u -> Expr.Sort (Level.Succ u)
  | _ -> Logger.err "failed inferring: %a" (TypeError expr) Expr.pp expr

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
