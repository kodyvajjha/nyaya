let () =
  (* let _file_outchannel =
       Format.formatter_of_out_channel (open_out "output.txt")
     in *)
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Debug);
  let result = Main.parse_from_file "test/parser/outparam_mock.export" in
  let env = Nyaya.Env.mk result in
  (* Check well-posedness of all declarations *)
  let all_well_posed = Nyaya.Tc.check_all_well_posed env in
  CCFormat.printf "All declarations well-posed: %b@." all_well_posed;

  Nyaya.Tc.typecheck env
