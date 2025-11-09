module List = CCList
module Array = CCArray

let export_dir = "../../../test/parser"

let format_test_name path =
  let basename = Filename.basename path in
  let len = String.length basename in
  if len > 7 && String.sub basename (len - 7) 7 = ".export" then (
    let prefix = String.sub basename 0 (len - 7) in
    "test/parser/" ^ prefix ^ ".source"
  ) else
    "test/parser/" ^ basename

let export_files () =
  Sys.readdir export_dir |> Array.to_list
  |> List.filter (fun f ->
         Filename.check_suffix f ".export"
         && f <> "init.export" && f <> "mutual.export" && f <> "mylist.export"
         && f <> "inductive.export")
  |> List.sort String.compare
  |> List.map (Filename.concat export_dir)

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
    export_files ()
    |> List.map (fun f ->
           Alcotest.test_case (format_test_name f) `Quick (run f))
  in
  Alcotest.run
    ~argv:[| "ignored"; "--tail-errors=0" |]
    "Nyaya_parser/typechecker"
    [ "export-files", cases ]
