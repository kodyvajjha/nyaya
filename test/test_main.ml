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

(* The nyaya binary, relative to the alcotest runtime cwd (_build/default/test).
   Driving the real executable -- rather than calling [check_env_verdict] in
   process -- exercises exactly the exit-code path the Lean Kernel Arena reads,
   so a green test here is a faithful proxy for a passing arena run. *)
let arena_bin = "../bin/main.exe"

(* Per-file wall-clock cap. A hung checker must fail its own case rather than
   stall the whole suite; the passing corpus checks in well under a second. *)
let arena_timeout_s = 30.0

(* Run [arena_bin] on [file] in arena mode and report how it terminated:
   [`Exit code] with the process exit code, or [`Timeout] if it was killed for
   exceeding [arena_timeout_s]. A signal/crash is reported as exit 3 (a checker
   bug), matching the entry point's own "anything but 0/1/2 is a bug" mapping. *)
let run_arena file : [ `Exit of int | `Timeout ] =
  let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let env = Array.append (Unix.environment ()) [| "NYAYA_ARENA=1" |] in
  let pid =
    Unix.create_process_env arena_bin [| arena_bin; file |] env Unix.stdin
      devnull devnull
  in
  Unix.close devnull;
  let deadline = Unix.gettimeofday () +. arena_timeout_s in
  let rec wait () =
    let reaped, status = Unix.waitpid [ Unix.WNOHANG ] pid in
    if reaped = 0 then
      if Unix.gettimeofday () > deadline then (
        (try Unix.kill pid Sys.sigkill with _ -> ());
        ignore (Unix.waitpid [] pid);
        `Timeout)
      else (
        Unix.sleepf 0.02;
        wait ())
    else
      match status with
      | Unix.WEXITED c -> `Exit c
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> `Exit 3
  in
  wait ()

(* Assert the checker's verdict on [file] against the arena outcome its corpus
   encodes: [good/] files must accept (0), [bad/] files must reject (1). A
   decline (2) is accepted either way -- the arena ignores declined tests for
   scoring, so it is never a local failure. Every other code (3 = checker bug,
   or a timeout) fails the case, with the offending value in the message. *)
let arena_case ~accepts file () =
  Logs.set_level None;
  match run_arena file with
  | `Timeout -> Alcotest.failf "%s: timed out after %.0fs" file arena_timeout_s
  | `Exit 2 -> () (* declined: ignored, not a failure *)
  | `Exit code ->
    if not (accepts code) then
      Alcotest.failf "%s: unexpected exit code %d" file code

let ndjson_cases subdir ~accepts =
  ndjson_files (Filename.concat ndjson_root subdir)
  |> List.map (fun f ->
         Alcotest.test_case (ndjson_test_name f) `Quick (arena_case ~accepts f))

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
      (* good/ must accept (exit 0); bad/ must reject (exit 1). *)
      "ndjson-arena-good", ndjson_cases "good" ~accepts:(fun c -> c = 0);
      "ndjson-arena-bad", ndjson_cases "bad" ~accepts:(fun c -> c = 1);
    ]
