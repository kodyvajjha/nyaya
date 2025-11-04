(* let export_dir = "../../../test/parser" *)

(* let export_files () =
   Sys.readdir export_dir |> Array.to_list
   |> List.filter (fun f ->
          Filename.check_suffix f ".export" && f <> "init.export")
   |> List.sort String.compare
   |> List.map (Filename.concat export_dir) *)

let export_files () =
  [
    "../../../test/parser/higher_order.export";
    "../../../test/parser/id.export";
    "../../../test/parser/id_fvar.export";
    "../../../test/parser/inductive.export";
    "../../../test/parser/lambda.export";
    "../../../test/parser/level_subst.export";
    (* "../../../test/parser/mutual.export"; *)
    "../../../test/parser/outparam_mock.export";
    "../../../test/parser/outparam_mock2.export";
    "../../../test/parser/self_contained.export";
    "../../../test/parser/universes.export";
  ]

let run filename () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Info);
  let result = Main.parse_from_file filename in
  let env = Nyaya.Env.mk result in
  (* Check well-posedness of all declarations *)
  let all_well_posed = Nyaya.Tc.check_all_well_posed env in
  CCFormat.printf "All declarations well-posed: %b@." all_well_posed;

  Nyaya.Tc.typecheck env

let () =
  let cases =
    export_files () |> List.map (fun f -> Alcotest.test_case f `Quick (run f))
  in
  Alcotest.run "Nyaya_parser/typechecker" [ "export-files", cases ]
