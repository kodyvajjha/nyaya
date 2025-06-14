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

module MakeLogger (Data : sig
  val header : string
end) =
struct
  module Format = CCFormat
  open CalendarLib

  let stamp_tag : (Date.t * Time.t) Logs.Tag.def =
    Logs.Tag.def "stamp" ~doc:"Time stamp"
      (CCFormat.Dump.pair (Printer.Date.fprint "%D") (Printer.Time.fprint "%T"))

  let stamp = Logs.Tag.(empty |> add stamp_tag (Date.today (), Time.now ()))

  let reporter ppf =
    let report _src level ~over k msgf =
      let k _ =
        over ();
        k ()
      in
      let with_stamp h _tags k ppf fmt =
        Format.kfprintf k ppf
          ("%a[%a] @[" ^^ fmt ^^ "@]@.")
          Logs.pp_header (level, h) (Printer.Time.fprint "%T") (Time.now ())
      in
      msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
    in
    Time_Zone.change Time_Zone.Local;
    { Logs.report }

  let info fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.info (fun m -> m "%s" str ~header:Data.header ~tags:stamp))
      fmt

  let warn fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.warn (fun m -> m "%s" str ~header:Data.header ~tags:stamp))
      fmt

  let err fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.err (fun m -> m "%s" str ~header:Data.header ~tags:stamp))
      fmt

  let debug fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.debug (fun m -> m "%s" str ~header:Data.header ~tags:stamp))
      fmt
end
