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
            m "@[<v 0>%s@,Exception: %s@]" msg
              (Printexc.to_string exn) ~header:Data.header);
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
    let depth = List.length !stack in
    if depth > max_path_entries then
      "..." ^ ">" ^ String.concat ">" entries
    else
      String.concat ">" entries

  let current_depth () = List.length !stack

  let enter (env : Data.env) (input : Data.input) : frame =
    let module Logger = (val Data.env_logger env) in
    let id = !next_id in
    let frame = { id; input } in
    incr next_id;
    stack := frame :: !stack;
    let depth = current_depth () in
    if depth > max_depth then (
      let msg =
        Printf.sprintf "[%s#%d] depth %d exceeds max %d, input=%s" Data.kind id
          depth max_depth (Data.input_summary input)
      in
      Logger.app "DEPTH LIMIT: %s" msg;
      raise (Depth_limit msg)
    );
    if not elide_ok then
      Logger.debug "[%s#%d d=%d p=%s] -> %s" Data.kind id depth
        (current_path ()) (Data.input_summary input);
    frame

  let leave_success (env : Data.env) (frame : frame) (output : Data.output) =
    let module Logger = (val Data.env_logger env) in
    stack := CCList.tl !stack;
    if not elide_ok then
      Logger.debug "[%s#%d d=%d p=%s] <- ok %s" Data.kind frame.id
        (current_depth ()) (current_path ())
        (Data.output_summary output)

  let leave_failure (env : Data.env) (frame : frame) (exn : exn) =
    let module Logger = (val Data.env_logger env) in
    stack := CCList.tl !stack;
    match exn with
    | Depth_limit _ -> () (* already logged once at the point of origin *)
    | _ ->
      Logger.debug "[%s#%d d=%d p=%s] !! %s input=%s" Data.kind frame.id
        (current_depth ()) (current_path ()) (Printexc.to_string exn)
        (Data.input_summary frame.input)

  let reset () =
    stack := [];
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
