let () =
  (* let _file_outchannel =
       Format.formatter_of_out_channel (open_out "output.txt")
     in *)
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Info);
  let result = Main.parse_from_file "test/parser/init.export" in
  let env = Nyaya.Env.mk result in
  Nyaya.Tc.typecheck env
