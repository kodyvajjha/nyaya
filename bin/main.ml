let () =
  (* let _file_outchannel =
       Format.formatter_of_out_channel (open_out "output.txt")
     in *)
  let open Nyaya_parser in
  Printexc.record_backtrace true;
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Info);
  (* Construct the AST from export file. *)
  let result = Main.parse_from_file "test/parser/init.export" in
  (* Construct the environment from the AST. *)
  let env = Nyaya.Env.mk result in
  (* Typecheck the environment. *)
  Nyaya.Tc.typecheck env
