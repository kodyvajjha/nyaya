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

module Logger = struct
  module Format = CCFormat

  let stamp_tag : Mtime.span Logs.Tag.def =
    Logs.Tag.def "stamp" ~doc:"Relative monotonic time stamp" Mtime.Span.pp

  let stamp c = Logs.Tag.(empty |> add stamp_tag (Mtime_clock.count c))

  let reporter ppf =
    let report _src level ~over k msgf =
      let k _ =
        over ();
        k ()
      in
      let with_stamp h tags k ppf fmt =
        let stamp =
          match tags with
          | None -> None
          | Some tags -> Logs.Tag.find stamp_tag tags
        in
        let dt =
          match stamp with
          | None -> Mtime.Span.zero
          | Some s -> s
        in
        Format.kfprintf k ppf
          ("%a[%a] @[" ^^ fmt ^^ "@]@.")
          Logs.pp_header (level, h) Mtime.Span.pp dt
      in
      msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
    in
    { Logs.report }

  let info c ~header fmt =
    CCFormat.ksprintf
      ~f:(fun str -> Logs.info (fun m -> m "%s" str ~header ~tags:(stamp c)))
      fmt

  let warn c ~header fmt =
    CCFormat.ksprintf
      ~f:(fun str -> Logs.warn (fun m -> m "%s" str ~header ~tags:(stamp c)))
      fmt

  let err c ~header fmt =
    CCFormat.ksprintf
      ~f:(fun str -> Logs.err (fun m -> m "%s" str ~header ~tags:(stamp c)))
      fmt

  let debug c ~header fmt =
    CCFormat.ksprintf
      ~f:(fun str -> Logs.debug (fun m -> m "%s" str ~header ~tags:(stamp c)))
      fmt
end
