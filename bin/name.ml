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
  Logs.info (fun m ->
      m "@[%a@]@."
        (CCHashtbl.pp ~pp_sep:CCFormat.newline CCFormat.(int) Nyaya.Name.pp)
        resolved_names);
  Logs.info (fun m ->
      m "@[%a@]@."
        (CCHashtbl.pp ~pp_sep:CCFormat.newline CCFormat.(int) Nyaya.Level.pp)
        resolve_levels)
