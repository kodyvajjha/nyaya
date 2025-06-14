let () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Info);

  let result = Main.parse_from_file "test/parser/init.export" in
  let env = Nyaya.Env.table result in
  CCFormat.printf "%a" Nyaya.Env.pp env

(* let () =
   let result = parse_from_file "test/parser/init.export" in
   let resolved_names = Nyaya.Name.table result in
   let resolve_levels = Nyaya.Level.table result in
   let resolve_exprs = Nyaya.Expr.table result in
   let resolve_decls = Nyaya.Env.table result in
   let resolve_rec_rules = Nyaya.Decl.Rec_rule.table result in
   Logs.info (fun m ->
       m "Done! Total names : %d" (Hashtbl.length resolved_names));
   Logs.info (fun m ->
       m "Done! Total levels : %d" (Hashtbl.length resolve_levels));
   Logs.info (fun m -> m "Done! Total exprs : %d" (Hashtbl.length resolve_exprs));
   Logs.info (fun m ->
       m "Done! Total declarations : %d" (Hashtbl.length resolve_decls));
   Logs.info (fun m ->
       m "Done! Total rec rules : %d" (Hashtbl.length resolve_rec_rules)) *)
