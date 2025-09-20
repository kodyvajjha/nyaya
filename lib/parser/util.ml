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

  let timestamp () =
    let now = Unix.gettimeofday () in
    let tm = Unix.localtime now in
    CCFormat.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec

  let stamp_tag : string Logs.Tag.def =
    Logs.Tag.def "stamp" ~doc:"Time stamp" CCFormat.pp_print_string

  let stamp = Logs.Tag.(empty |> add stamp_tag (timestamp ()))

  let reporter ppf =
    let report _src level ~over k msgf =
      let k _ =
        over ();
        k ()
      in
      let with_stamp h _tags k ppf fmt =
        Format.kfprintf k ppf
          ("%a[%s] @[" ^^ fmt ^^ "@]@.")
          Logs.pp_header (level, h) (timestamp ())
      in
      msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
    in
    { Logs.report }

  let info fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.info (fun m -> m "%s" str ~header:Data.header ~tags:stamp))
      fmt

  let success fmt =
    CCFormat.set_color_default true;
    CCFormat.with_color_ksf "green"
      ~f:(fun str ->
        Logs.info (fun m -> m "%s" str ~header:Data.header ~tags:stamp))
      fmt

  let warn fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.warn (fun m -> m "%s" str ~header:Data.header ~tags:stamp))
      fmt

  let err fmt e =
    CCFormat.set_color_default true;
    CCFormat.with_color_ksf "red"
      ~f:(fun str ->
        Logs.err (fun m -> m "%s" str ~header:Data.header ~tags:stamp);
        raise e)
      fmt

  let debug fmt =
    CCFormat.ksprintf
      ~f:(fun str ->
        Logs.debug (fun m -> m "%s" str ~header:Data.header ~tags:stamp))
      fmt

  let debugf pp x =
    Logs.debug (fun m -> m ~header:Data.header ~tags:stamp "%a" pp x)
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
