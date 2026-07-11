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
         Filename.check_suffix f ".export" && f <> "init.export")
  |> List.sort String.compare
  |> List.map (Filename.concat export_dir)

let run filename () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.std_formatter);
  Logs.set_level (Some Logs.Info);
  let result = Main.parse_from_file filename in
  let env = Nyaya.Env.mk result in
  Nyaya.Tc.typecheck env

(* --- New JSON (ndjson) export-format parser tests. ------------------- *)

(* Root of the ndjson corpus, relative to the alcotest runtime cwd
   (_build/default/test), mirroring [export_dir] above. *)
let ndjson_root = "../../../test"

(** Recursively gather every [.ndjson] file under [dir]. *)
let rec ndjson_files dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun f ->
         let p = Filename.concat dir f in
         if Sys.is_directory p then ndjson_files p
         else if Filename.check_suffix p ".ndjson" then [ p ]
         else [])

(** Test name relative to the repository's [test/] directory. *)
let ndjson_test_name path =
  let prefix = ndjson_root ^ "/" in
  let plen = String.length prefix in
  if String.length path >= plen && String.sub path 0 plen = prefix then
    "test/" ^ String.sub path plen (String.length path - plen)
  else
    path

(* Every file in the corpus -- whether it is a "good" (should typecheck) or
   "bad" (should be rejected by the kernel) sample -- is syntactically valid
   ndjson and must parse into a non-empty AST. This exercises the new parser
   against the full range of nodes the corpus contains. *)
let parse_ndjson filename () =
  Logs.set_level None;
  let ast = Nyaya_parser.Ndjson.parse_from_file filename in
  Alcotest.(check bool)
    "AST has items" true
    (List.length ast.Nyaya_parser.Ast.items > 0)

let ndjson_cases subdir =
  ndjson_files (Filename.concat ndjson_root subdir)
  |> List.map (fun f ->
         Alcotest.test_case (ndjson_test_name f) `Quick (parse_ndjson f))

let () =
  let cases =
    export_files ()
    |> List.map (fun f ->
           Alcotest.test_case (format_test_name f) `Quick (run f))
  in
  Alcotest.run
    ~argv:[| "ignored"; "--tail-errors=0" |]
    "Nyaya_parser/typechecker"
    [
      "export-files", cases;
      "ndjson-parse-good", ndjson_cases "good";
      "ndjson-parse-bad", ndjson_cases "bad";
    ]
