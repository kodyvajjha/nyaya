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

open Parser

exception Eof

let token buf =
  match%sedlex buf with
  | digit -> NAT (int_of_string (Sedlexing.Latin1.lexeme buf))
  | newline -> EOF
  | '.' -> PERIOD
  | _ -> failwith "Unexpected character"
