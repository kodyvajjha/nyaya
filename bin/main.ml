let () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Info);
  let result = Main.parse_from_file "test/parser/init.export" in
  let env = Nyaya.Env.mk result in
  (* Check well-posedness of all declarations *)
  let all_well_posed = Nyaya.Tc.check_all_well_posed env in
  CCFormat.printf "All declarations well-posed: %b@." all_well_posed;

  Nyaya.Tc.typecheck env
