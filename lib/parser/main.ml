module ParserLogger = Util.MakeLogger (struct
  let header = "Parser"
end)

let parse_from_file filename : Ast.t =
  ParserLogger.info "Opening file for parsing...";
  let ch = open_in filename in
  let lexbuf = Sedlexing.Utf8.from_channel ch in
  let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file in
  ParserLogger.info "Started parsing...";
  try parser lexer
  with Parser.Error ->
    Util.Location.handle_parser_error lexbuf;
    exit 1
