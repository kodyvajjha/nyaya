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

let hint_of_ast (h : Nyaya_parser.Ast.hint) =
  match h with
  | Nyaya_parser.Ast.HO -> Opaque
  | Nyaya_parser.Ast.HA -> Abbrev
  | Nyaya_parser.Ast.HR i -> Reg i

module Rec_rule = struct
  type t = {
    ctor_name: Name.t;
    ctor_num_args: int;
    value: Expr.t;
  }
  [@@deriving show]

  let table (ast : Nyaya_parser.Ast.t) : (Nyaya_parser.Ast.ridx, t) Hashtbl.t =
    let resolved_table : (int, t) Hashtbl.t =
      Hashtbl.create (CCList.length ast.items)
    in
    let name_table = Name.table ast in
    let expr_table = Expr.table ast in
    let rec_rule_table = Nyaya_parser.Ast.Hashed.rec_rules ast in
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
