let parse_from_file filename =
  let open Nyaya_parser in
  let ch = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file in
  let result =
    try parser lexer
    with Parser.Error ->
      Util.handle_parser_error lexbuf;
      exit 1
  in
  CCFormat.printf "%a@." Nyaya_parser.Ast.pp result;
  print_newline ();
  flush stdout

let () = parse_from_file "test/parser/axiom.export"
