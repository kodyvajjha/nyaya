let () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Debug);
  let result = Main.parse_from_file "test/parser/id.export" in
  let env = Nyaya.Env.mk result in
  (* CCFormat.printf "@[Random el: %a@." Nyaya.Expr.pp
     (Util.get_random_el expr_tbl); *)
  CCFormat.printf "%a@.@]"
    (CCHashtbl.pp ~pp_sep:CCFormat.(newline) Nyaya.Name.pp Nyaya.Decl.pp)
    env;

  (* Check well-posedness of all declarations *)
  let all_well_posed = Nyaya.Tc.check_all_well_posed env in
  CCFormat.printf "All declarations well-posed: %b@." all_well_posed;

  Nyaya.Tc.typecheck env
