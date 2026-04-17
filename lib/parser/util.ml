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
