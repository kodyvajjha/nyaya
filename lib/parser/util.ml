(* Util file stolen from Andrej Bauer's spartan-type-theory *)

type location =
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
