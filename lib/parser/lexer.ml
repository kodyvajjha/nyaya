(*
   All unique chars in Init.lean.export:

   ! # % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ { | } ~ ¬ · × ‹ › ↔ ∀ ∃ ∅ ∈ ∉ ∘ ∣ ∧ ∨ ∩ ∪ ≈ ≠ ≤ ≥ ⊂ ⊃ ⊆ ⊇ ⊕ ⊢ $ 0 ₀ 1 ₁ 2 ₂ 3 ₃ 4 ₄ 5 ₅ 6 ₆ 7 ₇ 8 ₈ 9 a A b B c C d D e E f F g G h H i I j J k K l L m M n N o O p P q Q r R s S t T u U v V w W x X y z Z α β γ δ ε ι μ ρ σ Σ φ ω
*)
(*
   Lexical structure of names:

      ident: atomic_ident | ident "." atomic_ident
      atomic_ident: atomic_ident_start atomic_ident_rest*
      atomic_ident_start: letterlike | "_" | escaped_ident_part
      letterlike: [a-zA-Z] | greek | coptic | letterlike_symbols
      greek: <[α-ωΑ-Ωἀ-῾] except for [λΠΣ]>
      coptic: [ϊ-ϻ]
      letterlike_symbols: [℀-⅏]
      escaped_ident_part: "«" [^«»\r\n\t]* "»"
      atomic_ident_rest: atomic_ident_start | [0-9'ⁿ] | subscript
      subscript: [₀-₉ₐ-ₜᵢ-ᵪ]
*)

let is_preamble = ref true

let is_els = ref false

let is_eln = ref false

let is_def = ref false

let digit = [%sedlex.regexp? '0' .. '9']

let newline = [%sedlex.regexp? '\n' | "\r\n"]

let chars = [%sedlex.regexp? Star ('a' .. 'z' | 'A' .. 'Z' | 0x00AC .. 0x22A2)]

(** Various other exceptions not covered above. *)
let excps =
  [%sedlex.regexp?
    ( '.' | '_' | '-' | '(' | '#' | ')' | '[' | ']' | '{' | '}' | '$' | '&'
    | '%' | '@' | '\'' | '=' | '>' | '<' | '?' | '!' | ':' | ';' | ',' | '\\'
    | '/' | '^' | '|' | '*' | '+' | '~' )]

let name_preamble = [%sedlex.regexp? Plus digit]

let name = [%sedlex.regexp? Star (digit | chars | excps)]

open Parser

exception Eof

let handle_lexer_error buf =
  let lexeme = Sedlexing.Utf8.lexeme buf in
  let pos = Util.Location.of_lex buf in
  let errstr =
    CCFormat.sprintf "Lexer error at %a: unexpected token '%s'"
      Util.Location.pp_location pos lexeme
  in
  failwith errstr

module Logger = Util.MakeLogger (struct
  let counter = Mtime_clock.counter ()

  let header = "Lexer"
end)

let rec token buf =
  match%sedlex buf with
  | newline ->
    Logger.debug "NEWLINE";
    is_els := false;
    is_eln := false;
    is_def := false;
    is_preamble := false;
    NL
  | _ ->
    if !is_preamble then
      token_preamble buf
    else
      token_body buf

and token_body buf =
  match%sedlex buf with
  | Plus digit ->
    if !is_els then (
      Logger.debug "Str Lit:%s" (Sedlexing.Utf8.lexeme buf);
      STRLITHEX (Sedlexing.Utf8.lexeme buf)
    ) else if !is_eln then (
      Logger.debug "Nat Lit : %s" (Sedlexing.Utf8.lexeme buf);
      NATLITHEX (Sedlexing.Utf8.lexeme buf)
    ) else (
      Logger.debug "Nat : %d" (int_of_string (Sedlexing.Utf8.lexeme buf));
      NAT (int_of_string (Sedlexing.Utf8.lexeme buf))
    )
  | '.' ->
    if !is_preamble then
      PERIOD
    else
      NAME (Sedlexing.Utf8.lexeme buf)
  (* Name Tokens *)
  | "#NS" ->
    Logger.debug "#NS";
    NSTOK
  | "#NI" ->
    Logger.debug "#NI";
    NITOK
  (* Info tokens *)
  | "#BD" -> BDTOK
  | "#BI" -> BITOK
  | "#BS" -> BSTOK
  | "#BC" -> BCTOK
  (* Level Tokens *)
  | "#US" -> USTOK
  | "#UM" -> UMTOK
  | "#UIM" -> UIMTOK
  | "#UP" -> UPTOK
  (* Expr tokens *)
  | "#ES" -> ESTOK
  | "#EV" -> EVTOK
  | "#EC" -> ECTOK
  | "#EA" -> EATOK
  | "#EL" -> ELTOK
  | "#EP" -> EPTOK
  | "#EZ" -> EZTOK
  | "#EJ" -> EJTOK
  | "#ELN" ->
    is_eln := true;
    Logger.debug "is_eln : %s" (string_of_bool !is_eln);
    ELNTOK
  | "#ELS" ->
    is_els := true;
    Logger.debug "is_els : %s" (string_of_bool !is_els);
    ELSTOK
  | "#EM" -> EMTOK
  (* Hint tokens *)
  | "R" ->
    if !is_def then
      RTOK
    else
      NAME (Sedlexing.Utf8.lexeme buf)
  | "A" -> ATOK
  | "O" -> OTOK
  (* Decl Tokens *)
  | "#AX" -> AXTOK
  | "#RR" -> RRTOK
  | "#DEF" ->
    is_def := true;
    DEFTOK
  | "#THM" -> THMTOK
  | "#QUOT" -> QUOTOK
  | "#OPAQ" -> OPQTOK
  | "#IND" -> INDTOK
  | "#REC" -> RECTOK
  | "#CTOR" -> CTORTOK
  | name ->
    if !is_els then (
      Logger.debug "Lexing: %s" (Sedlexing.Utf8.lexeme buf);
      STRLITHEX (Sedlexing.Utf8.lexeme buf)
    ) else
      NAME (Sedlexing.Utf8.lexeme buf)
  | Sub (white_space, '\n') ->
    Logger.debug "Other Whitespace";
    token buf
  | eof ->
    Logger.debug "End";
    EOF
  | _ -> handle_lexer_error buf

and token_preamble buf =
  match%sedlex buf with
  | "." -> PERIOD
  | digit -> Parser.NAT (int_of_string @@ Sedlexing.Utf8.lexeme buf)
  | _ -> handle_lexer_error buf
