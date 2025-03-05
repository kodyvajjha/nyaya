(* Minimal parser.mly *)

%{
 open Ast (* Assuming you have an AST module *)
%}

%token <int> NAT
%token EOF
%token PERIOD
%start file
%type <t> file

%%

file:
  | version EOF { {version=$1;items=[]} }

version: 
  | NAT PERIOD NAT PERIOD NAT {[$1;$3;$5]}
