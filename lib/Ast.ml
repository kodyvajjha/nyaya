type t = {
  version: int list;
  items: item list;
}

and item =
  | EName of Name.t
  | ELevel of Level.t
  | EExpr of Expr.t
  | ERecRule of Decl.rule
  | EDecl of Decl.t
