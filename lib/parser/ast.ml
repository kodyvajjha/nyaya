type nidx = int [@@deriving show]

type uidx = int [@@deriving show]

type eidx = int [@@deriving show]

type ridx = int [@@deriving show]

type name =
  | NSName of {
      nid: nidx;
      uid: uidx;
      str: string;
    }
  | NIName of {
      nid: nidx;
      uid: uidx;
      nat: int;
    }
[@@deriving show]

type level =
  | USLevel
  | UMLevel
  | UIMLevel
  | UPLevel
[@@deriving show]

type expr =
  | EVExpr
  | ESExpr of {
      eid: eidx;
      uid: uidx;
    }
  | ECExpr
  | EAExpr
  | ELExpr
  | EPExpr
  | EZExpr
  | EJExpr
  | ELNExpr
  | ELSExpr
  | EMExpr
[@@deriving show]

type rec_rule = {
  rid: ridx;
  ctorName: nidx;
  numFields: int;
  value: eidx;
}
[@@deriving show]

type decl =
  | Axiom of {
      name: nidx;
      expr: eidx;
      uparams: uidx list;
    }
  | Quotient
  | Definition
  | Theorem
  | Inductive
  | Constructor
  | Recursor
[@@deriving show]

type item =
  | EName of name
  | ELevel of level
  | EExpr of expr
  | ERecRule of rec_rule
  | EDecl of decl
[@@deriving show]

type t = {
  version: int list;
  items: item list;
}
[@@deriving show]
