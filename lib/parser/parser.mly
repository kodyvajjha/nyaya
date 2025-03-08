(* Minimal parser.mly *)

%{
 open Ast (* Assuming you have an AST module *)
%}

%token <int> NAT
%token <string> NAME
%token EOF
%token PERIOD
%token NSTOK
%token NITOK
%token AXTOK
%token ESTOK
%start file
%type <t> file

%%

file:
  | version items EOF { {version=$1;items=$2} }

version: 
  | NAT PERIOD NAT PERIOD NAT {[$1;$3;$5]}

items:
  | item items {$1 :: $2}
  | /* empty  */ { [] }

item: 
  | name {EName ($1)}
  /* | universe {ELevel ($1)} */
  | expr {EExpr ($1)}
  /* | recrule {ERecRule ($1)} */
  | decl {EDecl ($1)}


name: 
  | NAT NSTOK NAT NAME { NSName {nid = $1; uid = $3; str = $4} }
  | NAT NITOK NAT NAT { NIName {nid = $1; uid = $3; nat = $4} }

decl:
  | AXTOK NAT NAT {Axiom {name = $2; expr = $3; uparams = []}}

expr: 
  | NAT ESTOK NAT {ESExpr {eid = $1; uid = $3} } 