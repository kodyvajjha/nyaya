let () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.err_formatter);
  Logs.set_level (Some Logs.Info);
  (* Path to a new-format (ndjson) export file: taken from the command line if
     given, otherwise a default sample under test/good. *)
  let file =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "test/good/tutorial/001_basicDef.ndjson"
  in
  (* Construct the AST from the ndjson export file. *)
  let result = Ndjson.parse_from_file file in
  (* Construct the environment from the AST. *)
  let env = Nyaya.Env.mk result in
  (* Typecheck the environment. *)
  Nyaya.Tc.typecheck env
