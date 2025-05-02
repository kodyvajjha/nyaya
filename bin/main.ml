let parse_from_file filename =
  let open Nyaya_parser in
  let open Lexer.Logger in
  Logs.set_reporter (reporter Format.std_formatter);
  Logs.set_level (Some Logs.Info);

  let ch = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file in
  let result =
    try parser lexer
    with Parser.Error ->
      Util.Location.handle_parser_error lexbuf;
      exit 1
  in
  Logs.info (fun m -> m "%a@." Nyaya_parser.Ast.pp result);
  print_newline ();
  flush stdout

let () = parse_from_file "test/parser/init.export"
