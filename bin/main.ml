let () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.err_formatter);
  Logs.set_level (Some Logs.Info);
  (* Construct the AST from export file. *)
  let result = Main.parse_from_file "test/parser/init.export" in
  (* Construct the environment from the AST. *)
  let env = Nyaya.Env.mk result in
  (* Typecheck the environment. *)
  Nyaya.Tc.typecheck env
