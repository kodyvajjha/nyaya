let parse_from_file filename =
  let open Nyaya_parser in
  let open Lexer.Logger in
  Logs.set_reporter (reporter Format.std_formatter);
  Logs.set_level (Some Logs.Info);

  let ch = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file in
  try parser lexer
  with Parser.Error ->
    Util.Location.handle_parser_error lexbuf;
    exit 1

let () =
  let result = parse_from_file "test/parser/init.export" in
  let resolved_names = Nyaya.Name.table result in
  let resolve_levels = Nyaya.Level.table result in
  let resolve_exprs = Nyaya.Expr.table result in
  let resolve_decls = Nyaya.Env.table result in
  let resolve_rec_rules = Nyaya.Decl.Rec_rule.table result in
  Logs.info (fun m ->
      m "Done! Total names : %d" (Hashtbl.length resolved_names));
  Logs.info (fun m -> m "Done! Total exprs : %d" (Hashtbl.length resolve_exprs));
  Logs.info (fun m ->
      m "Done! Total levels : %d" (Hashtbl.length resolve_levels));
  Logs.info (fun m ->
      m "Done! Total declarations : %d" (Hashtbl.length resolve_decls));
  Logs.info (fun m ->
      m "Done! Total rec rules : %d" (Hashtbl.length resolve_rec_rules))
