(* Raw skeleton types of the Lean 4 export format. *)

type nidx = int [@@deriving show]

type uidx = int [@@deriving show]

type eidx = int [@@deriving show]

type ridx = int [@@deriving show]

type info =
  | IBD
  | IBI
  | IBS
  | IBC
[@@deriving show]

type hint =
  | HO
  | HA
  | HR of int
[@@deriving show]

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
  | USLevel of {
      uid1: uidx;
      uid2: uidx;
    }
  | UMLevel of {
      uid1: uidx;
      uid2: uidx;
      uid3: uidx;
    }
  | UIMLevel of {
      uid1: uidx;
      uid2: uidx;
      uid3: uidx;
    }
  | UPLevel of {
      uid: uidx;
      nid: nidx;
    }
[@@deriving show]

type expr =
  | EVExpr of {
      eid: eidx;
      num: int;
    }
  | ESExpr of {
      eid: eidx;
      uid: uidx;
    }
  | ECExpr of {
      eid: eidx;
      nid: nidx;
      uids: uidx list;
    }
  | EAExpr of {
      eid1: eidx;
      eid2: eidx;
      eid3: eidx;
    }
  | ELExpr of {
      eid1: eidx;
      info: info;
      nid: nidx;
      eid2: eidx;
      eid3: eidx;
    }
  | EPExpr of {
      eid1: eidx;
      info: info;
      nid: nidx;
      eid2: eidx;
      eid3: eidx;
    }
  | EZExpr of {
      eid1: eidx;
      info: info;
      nid: nidx;
      eid2: eidx;
      eid3: eidx;
      eid4: eidx;
    }
  | EJExpr of {
      eid1: eidx;
      nid: nidx;
      num: int;
      eid2: eidx;
    }
  | ELNExpr of {
      eid: eidx;
      num: int;
    }
  | ELSExpr of {
      eid: eidx;
      hexhex: string list;
    }
  | EMExpr of {
      eid1: eidx;
      mptr: unit option; (* TODO: what is this??? *)
      eid2: eidx;
    }
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
  | Quotient of {
      name: nidx;
      expr: eidx;
      uparams: uidx list;
    }
  | Definition of {
      name: nidx;
      expr: eidx;
      value: eidx;
      hint: hint option;
      uparams: uidx list;
    }
  | Opaque of {
      name: nidx;
      expr: eidx;
      value: eidx;
      uparams: uidx list;
    }
  | Theorem of {
      name: nidx;
      expr: eidx;
      value: eidx;
      uparams: uidx list;
    }
    (* TODO: Figure out what is up with inductives. *)
  | Inductive of int list
  | Constructor of {
      name: nidx;
      expr: eidx;
      parent_inductive: nidx;
      ctor_id: int;
      num_params: int;
      num_fields: int;
      uparams: uidx list;
    }
    (* Since the recursors  and inductives have complicated look-ahead behaviour, we dump everything to a list and then post-process at the next stage. *)
  | Recursor of int list
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
