type t = (Name.t, Decl.t) Hashtbl.t

let pp fpf t =
  CCFormat.fprintf fpf "@[%a@]"
    (CCHashtbl.pp Name.pp Decl.pp ~pp_sep:CCFormat.newline)
    t

let getter tbl key excp =
  match CCHashtbl.get tbl key with
  | Some v -> v
  | None -> failwith @@ CCFormat.sprintf "Could not find id %d in %s" key excp

open Nyaya_parser

let table (ast : Ast.t) : t =
  let resolved_table = Hashtbl.create (CCList.length ast.items) in
  let expr_table = Expr.table ast in
  let name_table = Name.table ast in
  let _level_table = Level.table ast in
  let decl_table = Ast.Hashed.decls ast in
  let rec_rule_table = Decl.Rec_rule.table ast in
  (* CCFormat.printf "@[%a@]@."
     CCFormat.(CCHashtbl.pp int Level.pp ~pp_sep:CCFormat.newline)
     level_table; *)
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
              uparams >|= fun id -> getter name_table id "name table in Axiom")
          in
          let ty = getter expr_table expr "expr table in Axiom" in
          Decl.Axiom { name; uparams; ty }
        | Ast.Decl.Quotient { name; expr; uparams } ->
          let name = getter name_table name "name table in Quot" in
          let uparams =
            CCList.(
              uparams >|= fun id -> getter name_table id "name table in Quot")
          in
          let ty = getter expr_table expr "expr table in Quot" in
          let info : Decl.decl_info = { name; uparams; ty } in
          Quot { info }
        | Ast.Decl.Definition { name; expr; value; hint; uparams } ->
          let name = getter name_table name "name table in Def" in
          let uparams =
            CCList.(
              uparams >|= fun id -> getter name_table id "name table in Def")
          in
          let ty = getter expr_table expr "expr table in Def" in
          let info : Decl.decl_info = { name; uparams; ty } in
          let value = getter expr_table value "expr table for value in Def" in
          let red_hint = Decl.hint_of_ast hint in
          Def { info; value; red_hint }
        | Ast.Decl.Opaque { name; expr; value; uparams } ->
          let name = getter name_table name "name table in Quot" in
          let uparams =
            CCList.(
              uparams >|= fun id -> getter name_table id "name table in Opaque")
          in
          let ty = getter expr_table expr "expr table in Quot" in
          let info : Decl.decl_info = { name; uparams; ty } in
          let value = getter expr_table value "expr table for value in Def" in
          Opaque { info; value }
        | Ast.Decl.Theorem { name; expr; value; uparams } ->
          let name = getter name_table name "name table in Quot" in
          let uparams =
            CCList.(
              uparams >|= fun id ->
              getter name_table id "level table in Theorem")
          in
          let ty = getter expr_table expr "expr table in Quot" in
          let info : Decl.decl_info = { name; uparams; ty } in
          let value = getter expr_table value "expr table for value in Def" in
          Thm { info; value }
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
          let name = getter name_table name "name table in Quot" in
          let uparams =
            CCList.(
              uparams >|= fun id ->
              getter name_table id "level table in Constructor")
          in
          let ty = getter expr_table expr "expr table in Quot" in
          let info : Decl.decl_info = { name; uparams; ty } in
          let inductive_name =
            getter name_table parent_inductive "name table in Ind"
          in
          Ctor { info; inductive_name; ctor_id; num_params; num_fields }
        | Ast.Decl.Inductive l ->
          (* CCFormat.printf "@[%a@]@." CCFormat.Dump.(list int) l; *)
          let arr = CCArray.of_list l in
          let total = CCArray.length arr in
          let name = getter name_table arr.(0) "name table in Ind" in
          let ty = getter expr_table arr.(1) "expr table in Ind" in
          let all_names = ref [] in
          let num_inductives = arr.(6) in
          for j = 0 to num_inductives - 1 do
            all_names :=
              !all_names
              @ [
                  getter name_table
                    arr.(j + 7)
                    "name table getting inductive names";
                ]
          done;
          let ctor_names = ref [] in
          let num_constructors = arr.(num_inductives + 7) in
          for i = 0 to num_constructors - 1 do
            ctor_names :=
              !ctor_names
              @ [
                  getter name_table
                    arr.(num_inductives + 8 + i)
                    "name table getting inductive names";
                ]
          done;
          let uparams =
            let remaining =
              CCList.map
                (fun x -> arr.(x))
                (CCArray.(num_inductives + num_constructors + 7 -- (total - 1))
                |> CCArray.to_list)
            in
            (* CCFormat.printf "@[%a@]@." CCFormat.Dump.(list int) remaining; *)
            CCList.(
              let+ id = remaining in
              getter name_table id "name table in inductives")
          in
          Inductive
            {
              info = { name; uparams; ty };
              is_recursive = CCBool.of_int arr.(2);
              is_nested = CCBool.of_int arr.(3);
              num_params = arr.(4);
              num_idx = arr.(5);
              all_names = CCList.rev !all_names;
              ctor_names = CCList.rev !ctor_names;
            }
        | Ast.Decl.Recursor l ->
          let arr = CCArray.of_list l in
          let name = getter name_table arr.(0) "name table in Rec" in
          let ty = getter expr_table arr.(1) "expr table in Rec" in
          let num_inductives = arr.(2) in
          let inductive_names = ref [] in
          for j = 0 to num_inductives - 1 do
            inductive_names :=
              !inductive_names
              @ [ getter name_table arr.(j + 3) "name table in Rec" ]
          done;
          (* CCFormat.printf "@[%a@.@]"
             CCFormat.Dump.(list Name.pp)
             !inductive_names; *)
          let num_params = arr.(3 + num_inductives) in
          let num_idx = arr.(4 + num_inductives) in
          let num_motives = arr.(5 + num_inductives) in
          let num_minors = arr.(6 + num_inductives) in
          let num_rules = arr.(7 + num_inductives) in
          let rules = ref [] in
          for i = 0 to num_rules - 1 do
            rules :=
              !rules
              @ [
                  getter rec_rule_table
                    arr.(8 + num_inductives + i)
                    "rec rule table in Rec";
                ]
          done;
          let is_k = arr.(8 + num_inductives + num_rules) in
          Rec
            {
              info = { name; uparams = []; ty };
              num_params;
              num_idx;
              num_motives;
              num_minors;
              (* TODO: resolve rec rules*)
              rules = CCList.rev !rules;
              is_K = CCBool.of_int is_k;
            }
      in

      (match resolved with
      | Some d ->
        Hashtbl.add resolved_table nm d;
        d
      | None -> failwith "Decl resolution failed")
  in
  Hashtbl.iter (fun nid _ -> ignore (resolve nid)) decl_table;
  resolved_table
