open Nyaya

let parse_from_file filename =
  let ch = open_in filename in
  try
    let lexbuf = Sedlexing.Utf8.from_channel ch in
    while true do
      let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
      let parser =
        MenhirLib.Convert.Simplified.traditional2revised Parser.file
      in
      let _result = parser lexer in
      print_newline ();
      flush stdout
    done
  with Lexer.Eof -> exit 0

let () = parse_from_file "test/parser/axiom.export"
