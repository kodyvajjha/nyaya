(* Minimal parser.mly *)

%{
 (* open Ast Assuming you have an AST module *)
%}

%token EOF

%start file
%type <unit> file

%%

file:
  | EOF { () }
