let () =
  let open Nyaya_parser in
  Logs.set_reporter (Lexer.Logger.reporter Format.err_formatter);
  Logs.set_level (Some Logs.Info);
  (* Path to a new-format (ndjson) export file: taken from the command line if
     given, otherwise a default sample under test/good. *)
  let file =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "test/good/tutorial/001_basicDef.ndjson"
  in
  (* Pick a parser by file format: [.ndjson] is the new JSON export format
     (arena / tutorial cases); anything else (e.g. [test/init.export], the
     legacy Lean 4 array export, or the [.source] parser fixtures) goes through
     the original menhir/sedlex parser. Both produce an [Ast.t], so the rest of
     the pipeline is identical. *)
  let parse_file (path : string) : Ast.t =
    if Filename.check_suffix path ".ndjson" then Ndjson.parse_from_file path
    else Main.parse_from_file path
  in
  match Sys.getenv_opt "NYAYA_ARENA" with
  | Some ("1" | "true" | "TRUE") ->
    (* Lean Kernel Arena mode: check the single export file at [file] (the arena
       passes its path via $IN) and communicate the verdict purely through the
       process exit code:
         0 = accept, 1 = reject, 2 = declined, anything else = a bug in nyaya.
       Diagnostics go to stderr; stdout is left clean. *)
    Logs.set_level (Some Logs.Error);
    let code =
      try
        let env = Nyaya.Env.mk (parse_file file) in
        match Nyaya.Tc.check_env_verdict env with
        | Nyaya.Tc.Accept -> 0
        | Nyaya.Tc.Reject reason ->
          Printf.eprintf "reject: %s\n" reason;
          1
        | Nyaya.Tc.Decline reason ->
          Printf.eprintf "decline: %s\n" reason;
          2
      with exn ->
        (* Parse failure, out-of-memory, stack overflow, or any unexpected
           leak: the arena treats a non-{0,1,2} code as a checker bug, which is
           exactly what these are -- not a verdict about the proof. *)
        Printf.eprintf "checker error: %s\n" (Printexc.to_string exn);
        3
    in
    exit code
  | _ ->
    (* Default discovery mode: build the environment and run the full
       declaration sweep (honours NYAYA_SWEEP_ALL / NYAYA_ONLY_DECL etc.). *)
    let result = parse_file file in
    let env = Nyaya.Env.mk result in
    Nyaya.Tc.typecheck env
