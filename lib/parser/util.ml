module Location = struct
  (* Stolen from Andrej Bauer's spartan-type-theory *)

  type t =
    | Location of Lexing.position * Lexing.position  (** delimited location *)
    | Nowhere  (** no location *)

  let of_lex lex =
    let l1, l2 = Sedlexing.lexing_positions lex in
    Location (l1, l2)

  let pp_location ppf loc =
    match loc with
    | Nowhere -> Format.fprintf ppf "unknown location"
    | Location (begin_pos, end_pos) ->
      let begin_char = begin_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let end_char = end_pos.Lexing.pos_cnum - begin_pos.Lexing.pos_bol in
      let begin_line = begin_pos.Lexing.pos_lnum in
      let filename = begin_pos.Lexing.pos_fname in

      if String.length filename != 0 then
        CCFormat.fprintf ppf "file %S, line %d, charaters %d-%d" filename
          begin_line begin_char end_char
      else
        CCFormat.fprintf ppf "line %d, characters %d-%d" begin_line begin_char
          end_char

  let handle_parser_error lexbuf =
    let pos = of_lex lexbuf in
    let lexeme = Sedlexing.Utf8.lexeme lexbuf in
    CCFormat.eprintf "Parser error at position %a: unexpected token '%s'\n"
      pp_location pos lexeme
end

module type LOGGER = sig
  val reporter : CCFormat.t -> Logs.reporter

  val info : ('a, Format.formatter, unit, unit) format4 -> 'a

  val app : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warn : ('a, Format.formatter, unit, unit) format4 -> 'a

  val err : ('a, Format.formatter, unit, 'b) format4 -> exn -> 'a

  val debug : ('a, Format.formatter, unit, unit) format4 -> 'a

  val debugf : (CCFormat.t -> 'a -> unit) -> 'a -> unit

  val infof : (CCFormat.t -> 'a -> unit) -> 'a -> unit

  val success : ('a, Format.formatter, unit, unit) format4 -> 'a
end

module MakeLogger (Data : sig
  val header : string
end) : LOGGER = struct
  module Format = CCFormat

  let reporter ppf =
    let report _src level ~over k msgf =
      let k _ =
        over ();
        k ()
      in
      let with_header h _tags k ppf fmt =
        Format.kfprintf k ppf
          ("%a @[" ^^ fmt ^^ "@]@.")
          Logs.pp_header (level, h)
      in
      msgf @@ fun ?header ?tags fmt -> with_header header tags k ppf fmt
    in
    { Logs.report }

  let info fmt =
    CCFormat.ksprintf
      ~f:(fun str -> Logs.info (fun m -> m "%s" str ~header:Data.header))
      fmt

  let app fmt =
    CCFormat.ksprintf
      ~f:(fun str -> Logs.app (fun m -> m "%s" str ~header:Data.header))
      fmt

  let success fmt =
    CCFormat.set_color_default true;
    CCFormat.with_color_ksf "green"
      ~f:(fun str -> Logs.info (fun m -> m "%s" str ~header:Data.header))
      fmt

  let warn fmt =
    CCFormat.ksprintf
      ~f:(fun str -> Logs.warn (fun m -> m "%s" str ~header:Data.header))
      fmt

  let err fmt exn =
    CCFormat.set_color_default true;
    CCFormat.with_color_ksf "red"
      ~f:(fun msg ->
        Logs.err (fun m ->
            m "@[<v 0>%s@,Exception: %s@]" msg (Printexc.to_string exn)
              ~header:Data.header);
        raise exn)
      fmt

  let debug fmt =
    CCFormat.ksprintf
      ~f:(fun str -> Logs.debug (fun m -> m "%s" str ~header:Data.header))
      fmt

  let debugf pp x =
    CCFormat.set_color_default true;
    Logs.debug (fun m -> m ~header:Data.header "%a" pp x)

  let infof pp x =
    CCFormat.set_color_default true;
    Logs.info (fun m -> m ~header:Data.header "%a" pp x)
end

module type TRACE_DATA = sig
  type env

  type input

  type output

  val env_logger : env -> (module LOGGER)

  val kind : string

  val elide_ok_env : string
  (** Name of the env var that controls whether successful trace lines are
      hidden. Truthy values are: [1], [true], [TRUE], [yes], [YES].

      When enabled, [enter] and [leave_success] logs are suppressed, while
      [leave_failure] logs remain visible. *)

  val max_depth_env : string
  (** Name of the env var that sets the maximum trace depth.  When the
      depth exceeds this limit, [enter] raises [Failure] with a diagnostic
      message.  Default (when unset): 1000. *)

  val input_summary : input -> string

  val output_summary : output -> string
end

exception Depth_limit of string

module MakeTrace (Data : TRACE_DATA) = struct
  type frame = {
    id: int;
    input: Data.input;
  }

  let stack : frame list ref = ref []

  (* Recursion depth, maintained as an O(1) counter. Kept in sync with [stack]
     but used in place of [List.length !stack] so the per-call depth-limit check
     does not cost O(depth) (which makes deep reductions O(depth^2)). *)
  let depth = ref 0

  (* Peak depth reached since the last [reset]; a diagnostic yardstick. *)
  let max_depth_seen = ref 0

  let next_id = ref 0

  let elide_ok =
    match Sys.getenv_opt Data.elide_ok_env with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false

  let max_depth =
    match Sys.getenv_opt Data.max_depth_env with
    | Some s -> (try int_of_string s with _ -> 2000)
    | None -> 2000

  let max_path_entries = 8

  let current_path () =
    (* Stack is newest-first; take the last max_path_entries frames *)
    let entries =
      CCList.take max_path_entries !stack
      |> List.rev
      |> CCList.map (fun frame -> string_of_int frame.id)
    in
    if !depth > max_path_entries then
      "..." ^ ">" ^ String.concat ">" entries
    else
      String.concat ">" entries

  let current_depth () = !depth

  let enter (env : Data.env) (input : Data.input) : frame =
    let module Logger = (val Data.env_logger env) in
    let id = !next_id in
    let frame = { id; input } in
    incr next_id;
    stack := frame :: !stack;
    incr depth;
    if !depth > !max_depth_seen then max_depth_seen := !depth;
    if !depth > max_depth then (
      let msg =
        Printf.sprintf "[%s#%d] depth %d exceeds max %d, input=%s" Data.kind id
          !depth max_depth (Data.input_summary input)
      in
      Logger.app "DEPTH LIMIT: %s" msg;
      raise (Depth_limit msg)
    );
    (* DEBUG-GATE: Data.input_summary/output_summary (Expr.pp, a non-
       memoized DAG-as-tree walk) must never be *computed* unless Debug
       logging is actually on. Passing it as a normal function argument to
       Logger.debug is NOT lazy -- OCaml evaluates function arguments
       eagerly, so the format string's %s placeholder forces the full
       pretty-print on every enter/leave call regardless of log level,
       before Logs' own (correctly lazy) level check ever runs. On a
       heavily-shared hash-consed DAG this can single-handedly OOM. See
       project_nyaya_oom_investigation memory / Vector.pmap_attach. *)
    if (not elide_ok) && Logs.level () = Some Logs.Debug then
      Logger.debug "[%s#%d d=%d p=%s] -> %s" Data.kind id !depth
        (current_path ()) (Data.input_summary input);
    frame

  let leave_success (env : Data.env) (frame : frame) (output : Data.output) =
    let module Logger = (val Data.env_logger env) in
    stack := CCList.tl !stack;
    decr depth;
    if (not elide_ok) && Logs.level () = Some Logs.Debug then
      Logger.debug "[%s#%d d=%d p=%s] <- ok %s" Data.kind frame.id
        (current_depth ()) (current_path ())
        (Data.output_summary output)

  let leave_failure (env : Data.env) (frame : frame) (exn : exn) =
    let module Logger = (val Data.env_logger env) in
    stack := CCList.tl !stack;
    decr depth;
    match exn with
    | Depth_limit _ -> () (* already logged once at the point of origin *)
    | _ ->
      if Logs.level () = Some Logs.Debug then
        Logger.debug "[%s#%d d=%d p=%s] !! %s input=%s" Data.kind frame.id
          (current_depth ()) (current_path ()) (Printexc.to_string exn)
          (Data.input_summary frame.input)

  (* Diagnostic accessors (see NYAYA_STATS). [calls] is the number of [enter]s
     since the last [reset] (i.e. non-memoized invocations for this decl);
     [peak_depth] the deepest recursion reached. *)
  let calls () = !next_id

  let peak_depth () = !max_depth_seen

  let reset () =
    stack := [];
    depth := 0;
    max_depth_seen := 0;
    next_id := 0
end

let get_random_el (tbl : (int, 'a) Hashtbl.t) : 'a =
  let el =
    let open CCRandom in
    let+ key = CCList.random_choose (CCHashtbl.keys_list tbl) in
    Hashtbl.find tbl key
  in
  el |> CCRandom.run

module Uid : sig
  type t = int

  val mk : unit -> t
end = struct
  type t = int

  let counter = ref 0

  let mk () =
    let id = !counter in
    incr counter;
    id
end

(** Per-run performance diagnostics for the checker, opt-in via [NYAYA_STATS=1].

    Records one row per checked declaration (reduction work, recursion depth,
    CPU time) and prints a totals line plus the heaviest declarations. The
    deterministic counters are the primary yardstick — reproducible across runs
    and machines, so a diff of two runs is signal, not noise — with CPU time as
    the real-cost cross-check.

    This module only aggregates and reports. The caller feeds each declaration's
    counters to {!end_decl}, keeping it decoupled from the checker's traces and
    memo tables. *)
module Stats = struct
  (** Whether stats collection is enabled ([NYAYA_STATS=1]). When [false],
      {!begin_decl}, {!end_decl} and {!report} are no-ops and callers pay
      nothing beyond the guards. *)
  let on =
    match Sys.getenv_opt "NYAYA_STATS" with
    | Some ("1" | "true" | "TRUE") -> true
    | _ -> false

  type decl_row = {
    name: string;
        (** Fully-qualified declaration name. Identity only, not a metric. *)
    whnf_misses: int;
        (** Non-memoized [whnf] invocations, i.e. weak-head reduction steps
            actually performed. The headline work metric. Lower is better. *)
    infer_misses: int;
        (** Non-memoized [infer] invocations, i.e. type-inference steps actually
            performed. Lower is better. *)
    defeq_calls: int;
        (** [isDefEq] invocations, i.e. definitional-equality checks attempted.
            Lower is better. *)
    delta: int;
        (** Successful one-step delta unfolds (a definition replaced by its
            body). The direct target of lazy-delta. Lower is better. *)
    peak_whnf: int;
        (** Deepest [whnf] recursion reached. Reduction the kernel keeps shallow
            shows up small here; runaway reduction spikes it (and can trip the
            depth limit). Lower is better. *)
    peak_infer: int;  (** Deepest [infer] recursion reached. Lower is better. *)
    peak_defeq: int;
        (** Deepest [isDefEq] recursion reached. Lower is better. *)
    whnf_hit: int;
        (** [whnf] memo hits, i.e. reductions the cache avoided. Read as a ratio
            against [whnf_misses] (the printed hit-rate): a higher share means
            more sharing was exploited. {b Higher is better}. *)
    cpu_ms: float;
        (** CPU milliseconds spent checking this declaration. The real-cost
            cross-check on the deterministic counters above. Lower is better. *)
  }
  (** One row of metrics for a single checked declaration. Every count is for
      that declaration alone (the checker resets its traces/memos between
      declarations). Unless noted, {b lower is better}: these are work a smarter
      algorithm (e.g. lazy-delta) should shrink. *)

  let rows : decl_row list ref = ref []

  (* CPU seconds at the start of the declaration currently being timed. *)
  let decl_t0 = ref 0.0

  (** Start timing the declaration about to be checked. Call after the caller
      has reset its per-declaration traces/counters. *)
  let begin_decl () = if on then decl_t0 := Sys.time ()

  (** Record the just-checked declaration's counters (call before the next
      per-declaration reset). The caller passes the per-declaration counts it
      collected; [cpu_ms] is derived from {!begin_decl}. *)
  let end_decl ~name ~whnf_misses ~infer_misses ~defeq_calls ~delta ~peak_whnf
      ~peak_infer ~peak_defeq ~whnf_hit () =
    if on then
      rows :=
        {
          name;
          whnf_misses;
          infer_misses;
          defeq_calls;
          delta;
          peak_whnf;
          peak_infer;
          peak_defeq;
          whnf_hit;
          cpu_ms = (Sys.time () -. !decl_t0) *. 1000.;
        }
        :: !rows

  (** Print the totals line and the [top] heaviest declarations (by
      [whnf_misses]) to stderr. No-op when stats are disabled. *)
  let report ?(top = 15) () =
    if on then (
      let rs = !rows in
      let n = List.length rs in
      let sum f = List.fold_left (fun a r -> a + f r) 0 rs in
      let sumf f = List.fold_left (fun a r -> a +. f r) 0. rs in
      let tot_whnf = sum (fun r -> r.whnf_misses) in
      let tot_infer = sum (fun r -> r.infer_misses) in
      let tot_defeq = sum (fun r -> r.defeq_calls) in
      let tot_delta = sum (fun r -> r.delta) in
      let tot_hit = sum (fun r -> r.whnf_hit) in
      let tot_ms = sumf (fun r -> r.cpu_ms) in
      let max_peak = List.fold_left (fun a r -> max a r.peak_whnf) 0 rs in
      let hit_rate =
        let d = tot_whnf + tot_hit in
        if d = 0 then
          0.0
        else
          100. *. float_of_int tot_hit /. float_of_int d
      in
      Printf.eprintf
        "\n\
         [STATS] %d decls | whnf-miss=%d infer-miss=%d defeq=%d delta=%d | \
         whnf-memo-hit=%.1f%% | peak-whnf-depth=%d | cpu=%.0fms\n"
        n tot_whnf tot_infer tot_defeq tot_delta hit_rate max_peak tot_ms;
      let by_whnf =
        List.sort (fun a b -> compare b.whnf_misses a.whnf_misses) rs
      in
      Printf.eprintf "[STATS] top %d declarations by whnf-miss:\n" (min top n);
      Printf.eprintf "        %-52s %10s %8s %8s %8s %9s\n" "declaration"
        "whnf-miss" "delta" "defeq" "peak-w" "cpu-ms";
      List.iteri
        (fun i r ->
          if i < top then
            Printf.eprintf "        %-52s %10d %8d %8d %8d %9.1f\n"
              (if String.length r.name <= 52 then
                 r.name
               else
                 String.sub r.name 0 49 ^ "...")
              r.whnf_misses r.delta r.defeq_calls r.peak_whnf r.cpu_ms)
        by_whnf;
      flush stderr
    )
end
