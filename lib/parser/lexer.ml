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

let digit = [%sedlex.regexp? '0' .. '9']

let newline = [%sedlex.regexp? '\n' | "\r\n"]

let chars =
  [%sedlex.regexp?
    Star
      ( 'a' .. 'z'
      | 'A' .. 'Z'
      (* Greek except lambda, Pi, Sigma *)
      | 0x03B1 .. 0x03C9
      (* α-ω *)
      | 0x0391 .. 0x03A9
      (* Α-Ω *)
      | 0x1F00 .. 0x1FFF
      (* ἀ-῾ *)
      (* Coptic *)
      | 0x03CA .. 0x03FB
      (* ϊ-ϻ *)
      (* Letterlike symbols *)
      | 0x2100 .. 0x214F (* ℀-⅏ *) )]

let subscript =
  [%sedlex.regexp?
    ( 0x2080 .. 0x2089
    (* ₀-ₙ subscript digits *)
    | 0x2090 .. 0x209C
    (* ₐ-ₜ subscript letters *)
    | 0x1D62 .. 0x1D6A (* ᵢ-ᵪ subscript letters *) )]

let excps = [%sedlex.regexp? '_' | '@']

let name = [%sedlex.regexp? Star (digit | chars | subscript | excps)]

open Parser

exception Eof

let rec token buf =
  match%sedlex buf with
  | Plus digit ->
    print_endline (Sedlexing.Utf8.lexeme buf);
    NAT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | '.' -> PERIOD
  (* Name Tokens *)
  | "#NS" ->
    print_endline "#NS";
    NSTOK
  | "#NI" ->
    print_endline "#NI";
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
  | "#ELN" -> ELNTOK
  | "#ELS" -> ELSTOK
  | "#EM" -> EMTOK
  (* Hint tokens *)
  | "O" -> OTOK
  | "R" -> RTOK
  | "A" -> ATOK
  (* Decl Tokens *)
  | "#AX" -> AXTOK
  | "#RR" -> RRTOK
  | "#DEF" -> DEFTOK
  | "#THM" -> THMTOK
  | "#QUOT" -> QUOTOK
  | "#IND" -> INDTOK
  | "#REC" -> RECTOK
  | "#CTOR" -> CTORTOK
  | newline ->
    print_endline "\t NEWLINE";
    NL
  | name -> Parser.NAME (Sedlexing.Utf8.lexeme buf)
  | Sub (white_space, '\n') ->
    print_endline "Other whitespace";
    token buf
  | eof ->
    print_endline "\tEnd";
    EOF
  | _ ->
    let lexeme = Sedlexing.Utf8.lexeme buf in
    let pos = Util.of_lex buf in
    let errstr =
      CCFormat.sprintf "Lexer error at %a: unexpected token '%s'"
        Util.pp_location pos lexeme
    in
    failwith errstr
