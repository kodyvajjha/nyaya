[@@@warning "-27"]

open Nyaya_parser

let getter tbl key excp =
  match CCHashtbl.get tbl key with
  | Some v -> v
  | None -> failwith @@ CCFormat.sprintf "Could not find id %d in %s" key excp

let table (ast : Ast.t) : (Name.t, Decl.t) Hashtbl.t =
  let resolved_table = Hashtbl.create (CCList.length ast.items) in
  let expr_table = Expr.table ast in
  let name_table = Name.table ast in
  let level_table = Level.table ast in
  let decl_table = Ast.Hashed.decls ast in
  let resolve (nid : int) =
    let nm = getter name_table nid "name table at top" in
    match Hashtbl.find_opt resolved_table nm with
    | Some decl -> decl
    | None ->
      let resolved =
        let open CCOption in
        let+ decl = Hashtbl.find_opt decl_table nid in

        match decl with
        | Ast.Decl.Axiom { name; expr; uparams } ->
          let name = getter name_table name "name table in Axiom" in
          let uparams =
            CCList.(
              uparams >|= fun id -> getter level_table id "level table in Axiom")
          in
          let ty = getter expr_table expr "expr table in Axiom" in
          Decl.Axiom { name; uparams; ty }
        | Ast.Decl.Quotient { name; expr; uparams } ->
          let name = getter name_table name "name table in Quot" in
          let uparams =
            CCList.(
              uparams >|= fun id -> getter level_table id "level table in Quot")
          in
          let ty = getter expr_table expr "expr table in Quot" in
          let info : Decl.decl_info = { name; uparams; ty } in
          Quot { info }
        | Ast.Decl.Definition { name; expr; value; hint; uparams } ->
          let name = getter name_table name "name table in Quot" in
          let uparams =
            CCList.(
              uparams >|= fun id -> getter level_table id "level table in Quot")
          in
          let ty = getter expr_table expr "expr table in Quot" in
          let info : Decl.decl_info = { name; uparams; ty } in
          let value = getter expr_table value "expr table for value in Def" in
          let red_hint = Decl.hint_of_ast hint in
          Def { info; value; red_hint }
        | Ast.Decl.Opaque { name; expr; value; uparams } ->
          let name = getter name_table name "name table in Quot" in
          let uparams =
            CCList.(
              uparams >|= fun id -> getter level_table id "level table in Quot")
          in
          let ty = getter expr_table expr "expr table in Quot" in
          let info : Decl.decl_info = { name; uparams; ty } in
          let value = getter expr_table value "expr table for value in Def" in
          Opaque { info; value }
        | Ast.Decl.Theorem { name; expr; value; uparams } -> assert false
        | Ast.Decl.Inductive l -> assert false
        | Ast.Decl.Constructor
            {
              name;
              expr;
              parent_inductive;
              ctor_id;
              num_params;
              num_fields;
              uparams;
            } ->
          assert false
        | Ast.Decl.Recursor l -> assert false
      in

      (match resolved with
      | Some d ->
        Hashtbl.add resolved_table nm d;
        d
      | None -> failwith "Decl resolution failed")
  in
  Hashtbl.iter (fun nid _ -> ignore (resolve nid)) decl_table;
  resolved_table
