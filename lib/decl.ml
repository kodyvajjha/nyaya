open Nyaya_parser

module Logger = Util.MakeLogger (struct
  let header = "Decl"
end)

type decl_info = {
  name: Name.t;
  uparams: Name.t list;
  ty: Expr.t;
}
[@@deriving show]

type hint =
  | Abbrev
  | Opaque
  | Reg of int
[@@deriving show]

let hint_of_ast (h : Ast.hint) =
  match h with
  | Ast.HO -> Opaque
  | Ast.HA -> Abbrev
  | Ast.HR i -> Reg i

module Rec_rule = struct
  module Logger = Util.MakeLogger (struct
    let header = "Rec rule"
  end)

  type t = {
    ctor_name: Name.t;
    ctor_num_args: int;
    value: Expr.t;
  }
  [@@deriving show]

  let table name_table expr_table (ast : Ast.t) : (Ast.ridx, t) Hashtbl.t =
    let resolved_table : (int, t) Hashtbl.t =
      Hashtbl.create (CCList.length ast.items)
    in
    let rec_rule_table = Ast.Hashed.rec_rules ast in
    let resolve (ridx : int) =
      match Hashtbl.find_opt resolved_table ridx with
      | Some rr -> rr
      | None ->
        let resolved : t =
          match Hashtbl.find_opt rec_rule_table ridx with
          | Some { ctorName; numFields; value; _ } ->
            let ctor_name = Hashtbl.find name_table ctorName in
            let value = Hashtbl.find expr_table value in
            { ctor_name; ctor_num_args = numFields; value }
          | None -> failwith "can't find that rec rule"
        in
        Hashtbl.add resolved_table ridx resolved;
        resolved
    in
    Hashtbl.iter (fun rid _ -> ignore (resolve rid)) rec_rule_table;
    Logger.info "Finished resolving rec rules. Total number : %d"
      (Hashtbl.length rec_rule_table);
    resolved_table
end

type t =
  | Axiom of decl_info
  | Def of {
      info: decl_info;
      value: Expr.t;
      red_hint: hint;
    }
  | Thm of {
      info: decl_info;
      value: Expr.t;
    }
  | Opaque of {
      info: decl_info;
      value: Expr.t;
    }
  | Quot of { info: decl_info }
  | Inductive of {
      info: decl_info;
      is_reflexive: bool;
      is_recursive: bool;
      num_nested: int;
      num_params: int;
      num_idx: int;
      all_names: Name.t list;
      ctor_names: Name.t list;
    }
  | Ctor of {
      info: decl_info;
      inductive_name: Name.t;
      ctor_id: int;
      num_params: int;
      num_fields: int;
    }
  | Rec of {
      info: decl_info;
      num_params: int;
      num_idx: int;
      num_motives: int;
      num_minors: int;
      rules: Rec_rule.t list;
      is_K: bool;
    }
[@@deriving show]

(** Helper to extract decl_info from any declaration *)
let get_decl_info (decl : t) : decl_info =
  match decl with
  | Axiom info -> info
  | Def { info; _ } -> info
  | Thm { info; _ } -> info
  | Opaque { info; _ } -> info
  | Quot { info } -> info
  | Inductive { info; _ } -> info
  | Ctor { info; _ } -> info
  | Rec { info; _ } -> info

let get_type (decl : t) =
  let info = get_decl_info decl in
  info.ty

let get_value (decl : t) =
  match decl with
  | Def { value; _ } -> Some value
  | Thm { value; _ } -> Some value
  | Opaque { value; _ } -> Some value
  (* TODO: This needs some figuring out... *)
  (* | Inductive { info; _ } -> Some info.ty *)
  (* | Axiom { ty; _ } -> Some ty *)
  | d ->
    Logger.debug "@[Warning: trying to extract value of declaration : @[%a@]@]"
      pp d;
    None

let get_uparams (decl : t) =
  match decl with
  | Axiom info -> info.uparams
  | Def { info; _ } -> info.uparams
  | Thm { info; _ } -> info.uparams
  | Opaque { info; _ } -> info.uparams
  | Quot { info } -> info.uparams
  | Inductive { info; _ } -> info.uparams
  | Ctor { info; _ } -> info.uparams
  | Rec { info; _ } -> info.uparams

let get_inductive_ctors (decl : t) =
  match decl with
  | Inductive { ctor_names; _ } -> ctor_names
  | _ ->
    Logger.err "Failed to collect constructors of non-inductive declaration."
      (Failure "")
