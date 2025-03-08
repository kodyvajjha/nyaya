(*
   All unique chars in Init.lean.export:

   ! # % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ { | } ~ ¬ · × ‹ › ↔ ∀ ∃ ∅ ∈ ∉ ∘ ∣ ∧ ∨ ∩ ∪ ≈ ≠ ≤ ≥ ⊂ ⊃ ⊆ ⊇ ⊕ ⊢ $ 0 ₀ 1 ₁ 2 ₂ 3 ₃ 4 ₄ 5 ₅ 6 ₆ 7 ₇ 8 ₈ 9 a A b B c C d D e E f F g G h H i I j J k K l L m M n N o O p P q Q r R s S t T u U v V w W x X y z Z α β γ δ ε ι μ ρ σ Σ φ ω
*)

let line_num = ref 1

let update_line lexbuf =
  incr line_num;
  Sedlexing.new_line lexbuf

let digit = [%sedlex.regexp? '0' .. '9']

let newline = [%sedlex.regexp? '\n' | "\r\n"]

let name = [%sedlex.regexp? xid_start, Star ('-' | '|' | xid_continue)]

open Parser

exception Eof

let rec token buf =
  match%sedlex buf with
  | digit ->
    print_endline (Sedlexing.Utf8.lexeme buf);
    NAT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | '.' -> PERIOD
  | "#NS" ->
    print_endline "#NS";
    NSTOK
  | "#NI" ->
    print_endline "#NI";
    NITOK
  | "#ES" ->
    print_endline "#ES";
    ESTOK
  | "#AX" ->
    print_endline "#AX";
    AXTOK
  | white_space ->
    print_endline "\tWhitespace";
    token buf
  | eof ->
    print_endline "\tEnd";
    EOF
  | name -> Parser.NAME (Sedlexing.Utf8.lexeme buf)
  | any ->
    print_endline "Other";
    token buf
  | _ ->
    let lexeme = Sedlexing.Utf8.lexeme buf in
    failwith ("Unexpected character:" ^ lexeme)
