let export_dir = "../../../test/parser"

let export_files () =
  Sys.readdir export_dir |> Array.to_list
  |> List.filter (fun f ->
         Filename.check_suffix f ".export" && f <> "init.export")
  |> List.sort String.compare
  |> List.map (Filename.concat export_dir)

let run_in_subprocess exe file () =
  let cmd = Printf.sprintf "%s %s" (Filename.quote exe) (Filename.quote file) in
  match Sys.command cmd with
  | 0 -> ()
  | n -> Alcotest.failf "Subprocess failed (%d) for %s" n file

let () =
  let files = export_files () in
  Fmt.pr "Discovered %d exports:@\n%a@." (List.length files)
    Fmt.(list ~sep:cut string)
    files;
  let exe = "../../../bin/main.exe" in
  let cases =
    export_files ()
    |> List.map (fun f -> Alcotest.test_case f `Quick (run_in_subprocess exe f))
  in
  Alcotest.run "Nyaya_parser/typechecker" [ "export-files", cases ]
