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
        Format.kfprintf k ppf ("%a @[" ^^ fmt ^^ "@]@.")
          Logs.pp_header (level, h)
      in
      msgf @@ fun ?header ?tags fmt -> with_header header tags k ppf fmt
    in
    { Logs.report }

  let info fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.info (fun m -> m "%s" str ~header:Data.header))
      fmt

  let app fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.app (fun m -> m "%s" str ~header:Data.header))
      fmt

  let success fmt =
    CCFormat.set_color_default true;
    CCFormat.with_color_ksf "green"
      ~f:(fun str ->
        Logs.info (fun m -> m "%s" str ~header:Data.header))
      fmt

  let warn fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.warn (fun m -> m "%s" str ~header:Data.header))
      fmt

  let err fmt exn =
    (* Grab backtrace for the most recently raised exception *)
    let bt = Printexc.get_raw_backtrace () in
    let bt_s = Printexc.raw_backtrace_to_string bt in

    CCFormat.set_color_default true;
    CCFormat.with_color_ksf "red"
      ~f:(fun msg ->
        (* Log the original message, the exception, and the backtrace *)
        Logs.err (fun m ->
            m "@[<v 0>%s@,Exception: %s@,@[<v 2>Backtrace:@,%s@]@]" msg
              (Printexc.to_string exn) bt_s ~header:Data.header);
        (* Re-raise, preserving backtrace *)
        Printexc.raise_with_backtrace exn bt)
      fmt

  let debug fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.debug (fun m -> m "%s" str ~header:Data.header))
      fmt

  let debugf pp x =
    CCFormat.set_color_default true;
    Logs.debug (fun m -> m ~header:Data.header "%a" pp x)

  let infof pp x =
    CCFormat.set_color_default true;
    Logs.info (fun m -> m ~header:Data.header "%a" pp x)
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

module MakeRecTrace (Cfg : sig
  type ctx
  type input

  val tag : string
  val enabled_env : string option
  val elide_ok_env : string option
  val only_env : string option
  val summarize : input -> string
  val log : ctx -> string -> unit
end) = struct
  type frame = {
    id: int;
    input: Cfg.input;
    enabled: bool;
  }

  let stack : frame list ref = ref []

  let next_id = ref 0

  let parse_bool_env k =
    match Sys.getenv_opt k with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false

  let enabled =
    match Cfg.enabled_env with
    | None -> true
    | Some k -> parse_bool_env k

  let elide_ok =
    match Cfg.elide_ok_env with
    | None -> false
    | Some k -> parse_bool_env k

  let only =
    match Cfg.only_env with
    | None -> None
    | Some k -> Sys.getenv_opt k

  let contains_substring ~needle haystack =
    let needle_len = String.length needle in
    needle_len > 0
    &&
    let rec go i =
      if i + needle_len > String.length haystack then
        false
      else if String.sub haystack i needle_len = needle then
        true
      else
        go (i + 1)
    in
    go 0

  let current_path () =
    !stack
    |> List.rev |> CCList.map (fun frame -> string_of_int frame.id)
    |> String.concat ">"

  let parent_enabled () = List.exists (fun frame -> frame.enabled) !stack

  let should_enable input =
    enabled
    &&
    match only with
    | None -> true
    | Some needle when needle = "" -> true
    | Some needle ->
      parent_enabled () || contains_substring ~needle (Cfg.summarize input)

  let enter (ctx : Cfg.ctx) (input : Cfg.input) : frame =
    let id = !next_id in
    incr next_id;
    let frame = { id; input; enabled = should_enable input } in
    stack := frame :: !stack;
    if frame.enabled && not elide_ok then
      Cfg.log ctx
        (CCFormat.sprintf "[%s#%d d=%d p=%s] -> %s" Cfg.tag id
           (List.length !stack - 1) (current_path ()) (Cfg.summarize input));
    frame

  let step (ctx : Cfg.ctx) (frame : frame) (label : string) (input : Cfg.input) : unit =
    if frame.enabled then
      Cfg.log ctx
        (CCFormat.sprintf "[%s#%d p=%s] .. %s %s" Cfg.tag frame.id
           (current_path ()) label (Cfg.summarize input))

  let branch (ctx : Cfg.ctx) (frame : frame) (label : string) : unit =
    if frame.enabled then
      Cfg.log ctx
        (CCFormat.sprintf "[%s#%d p=%s] .. branch %s" Cfg.tag frame.id
           (current_path ()) label)

  let leave (ctx : Cfg.ctx) (frame : frame) (outcome : string) : unit =
    stack := CCList.tl !stack;
    if frame.enabled && not elide_ok then
      Cfg.log ctx
        (CCFormat.sprintf "[%s#%d d=%d p=%s] <- %s" Cfg.tag frame.id
           (List.length !stack) (current_path ()) outcome)

  let leave_failure (ctx : Cfg.ctx) (frame : frame) (exn : exn) : unit =
    stack := CCList.tl !stack;
    if frame.enabled then
      Cfg.log ctx
        (CCFormat.sprintf "[%s#%d p=%s] !! %s input=%s" Cfg.tag frame.id
           (current_path ()) (Printexc.to_string exn) (Cfg.summarize frame.input))

  let reset () =
    stack := [];
    next_id := 0
end
