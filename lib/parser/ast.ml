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

module Name = struct
  type t =
    | NSName of {
        nid1: nidx;
        nid2: nidx;
        str: string;
      }
    | NIName of {
        nid1: nidx;
        nid2: nidx;
        nat: int;
      }
  [@@deriving show]
end

module Level = struct
  type t =
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
end

module Expr = struct
  type t =
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
        num: string;
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
end

type rec_rule = {
  rid: ridx;
  ctorName: nidx;
  numFields: int;
  value: eidx;
}
[@@deriving show]

module Decl = struct
  type t =
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
end

module Item = struct
  type t =
    | EName of Name.t
    | ELevel of Level.t
    | EExpr of Expr.t
    | ERecRule of rec_rule
    | EDecl of Decl.t
  [@@deriving show]

  let get_name item =
    match item with
    | EName nm -> Some nm
    | _ -> None

  let get_level item =
    match item with
    | ELevel lvl -> Some lvl
    | _ -> None

  let is_expr item =
    match item with
    | EExpr expr -> Some expr
    | _ -> None

  let is_decl item =
    match item with
    | EDecl dcl -> Some dcl
    | _ -> None

  let is_rec_rule item =
    match item with
    | ERecRule rr -> Some rr
    | _ -> None
end

type t = {
  version: int list;
  items: Item.t list;
}
[@@deriving show]

module Hashed = struct
  (** Return a Hashtbl of names with nids as keys. *)
  let names ast : (nidx, Name.t) Hashtbl.t =
    let names = CCList.filter_map Item.get_name ast.items in
    let raw_table = Hashtbl.create (List.length names) in
    CCList.iter
      (fun name ->
        let nid =
          match name with
          | Name.NSName { nid1; _ } -> nid1
          | Name.NIName { nid1; _ } -> nid1
        in
        Hashtbl.add raw_table nid name)
      names;
    raw_table

  (** Return a Hashtbl of levels with uids as keys. *)
  let levels ast : (uidx, Level.t) Hashtbl.t =
    let levels = CCList.filter_map Item.get_level ast.items in

    let raw_table = Hashtbl.create (List.length levels) in
    CCList.iter
      (fun (level : Level.t) ->
        let uid =
          match level with
          | Level.USLevel { uid1; _ } -> uid1
          | Level.UMLevel { uid1; _ } -> uid1
          | Level.UIMLevel { uid1; _ } -> uid1
          | Level.UPLevel { uid; _ } -> uid
        in

        Hashtbl.add raw_table uid level)
      levels;
    raw_table
end
