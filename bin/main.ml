(* let () =
   let open Nyaya_parser in
   Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
   Logs.set_level (Some Logs.Info);

   let result = Main.parse_from_file "test/parser/init.export" in

   let env = Nyaya.Env.mk result in
   CCFormat.printf "%a" Nyaya.Env.pp env *)

let () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Error);
  let result = Main.parse_from_file "test/parser/universes.export" in
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

(*
   let () =
     let open Nyaya.Level in
     let open Nyaya.Name in
     let u = Str (Anon, "u") in
     let v = Str (Anon, "v") in
     let l1 = Max (Param u, IMax (Param u, Succ (Param v))) in
     let _l2 = subst l1 (Str (Anon, "u")) Zero in
     let l3 = Max (Param u, IMax (Succ Zero, Zero)) in
     CCFormat.printf "Before : %a @. After : %a" Nyaya.Level.pp l1 Nyaya.Level.let open CCRandom inpp
       (simplify l3) *)
