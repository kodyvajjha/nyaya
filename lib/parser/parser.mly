(* Minimal parser.mly *)

%{
 open Ast (* Assuming you have an AST module *)
%}

%token <int> NAT
%token <string> NAME
%token <string> STRLITHEX
%token <string> NATLITHEX

%token EOF
%token PERIOD
%token NL 
// Name tokens
%token NSTOK
%token NITOK
// Info tokens
%token BDTOK
%token BCTOK 
%token BITOK 
%token BSTOK 
// Expr tokens
%token ESTOK
%token EVTOK
%token ELTOK
%token ECTOK
%token EATOK
%token EPTOK 
%token EZTOK 
%token EJTOK 
%token ELNTOK 
%token ELSTOK 
%token EMTOK
// Universe tokens
%token USTOK
%token UMTOK
%token UIMTOK
%token UPTOK 
// Recrule token
%token RRTOK
// Hint tokens
%token OTOK 
%token ATOK 
%token RTOK 
// Decl tokens
%token AXTOK
%token DEFTOK
%token THMTOK
%token QUOTOK
%token INDTOK 
%token RECTOK
%token CTORTOK
%token OPQTOK
%start file
%type <t> file

%%

file:
  | version NL items EOF { {version=$1;items=$3} }

version: 
  | NAT PERIOD NAT PERIOD NAT {[$1;$3;$5]}

uparams: 
  | NAT uparams {$1 :: $2}
  | /* empty */ { []  }

indrecparams:
  | NAT indrecparams {$1 :: $2}
  | /* empty */ { [] }
  
items:
  | item          {[$1]}
  | item NL items {$1 :: $3}
  | /* empty  */  {[]}

item: 
  | name {Item.EName ($1)}
  | universe {Item.ELevel ($1)} 
  | expr {Item.EExpr ($1)}
  | recrule {Item.ERecRule ($1)} 
  | decl {Item.EDecl ($1)}


name: 
  | NAT NSTOK NAT NAME { Name.NSName {nid1 = $1; nid2 = $3; str = $4} }
  | NAT NITOK NAT NAT { Name.NIName {nid1 = $1; nid2 = $3; nat = $4} }


universe:
  | NAT USTOK NAT { Level.USLevel {uid1 = $1; uid2 = $3}}
  | NAT UMTOK NAT NAT {Level.UMLevel {uid1 = $1; uid2 = $3; uid3 = $4}}
  | NAT UIMTOK NAT NAT {Level.UIMLevel {uid1 = $1; uid2 = $3; uid3=$4}}
  | NAT UPTOK NAT {Level.UPLevel {uid = $1; nid = $3}}
  

hint:
  | OTOK  { HO}
  | ATOK  { HA}
  | RTOK NAT { (HR ($2))}

decl:
  | AXTOK NAT NAT uparams {Axiom {name = $2; expr = $3; uparams = $4}}
  | QUOTOK NAT NAT uparams {Quotient {name=$2;expr=$3;uparams=$4}}
  | DEFTOK NAT NAT NAT hint uparams {Definition {name = $2; expr = $3; value=$4; hint = $5; uparams = $6}} 
  | OPQTOK NAT NAT NAT uparams {Opaque {name = $2; expr = $3; value=$4; uparams = $5}} 
  | THMTOK NAT NAT NAT uparams {Theorem {name = $2; expr=$3; value =$4;uparams=$5}} 
  | INDTOK indrecparams {Inductive ($2)}
  | CTORTOK NAT NAT NAT NAT NAT NAT uparams {Constructor {name=$2; expr=$3;parent_inductive=$4; ctor_id=$5; num_params=$6;num_fields=$7;uparams=$8}}
  | RECTOK indrecparams {Recursor ($2)}

info:
  | BDTOK {IBD}
  | BITOK {IBI} 
  | BSTOK {IBS}
  | BCTOK {IBC}

hexhex: 
  | /* empty */      {[]}
  | STRLITHEX hexhex {$1 :: $2}

expr: 
  | NAT EVTOK NAT {EVExpr {eid=$1;num=$3}}
  | NAT ESTOK NAT {ESExpr {eid = $1; uid = $3} } 
  | NAT ECTOK NAT uparams {ECExpr {eid = $1; nid=$3; uids=$4}}
  | NAT EATOK NAT NAT {EAExpr {eid1=$1;eid2=$3;eid3=$4}}
  | NAT ELTOK info NAT NAT NAT {ELExpr {eid1=$1;info=$3;nid=$4;eid2=$5; eid3=$6}}
  | NAT EPTOK info NAT NAT NAT {EPExpr {eid1=$1;info=$3;nid=$4;eid2=$5;eid3=$6}}
  | NAT EZTOK NAT NAT NAT NAT {EZExpr {eid1=$1;nid=$3;eid2=$4;eid3=$5; eid4=$6}}
  | NAT EJTOK NAT NAT NAT {EJExpr {eid1=$1;nid=$3;num=$4;eid2=$5}}
  | NAT ELNTOK NATLITHEX {ELNExpr {eid=$1;num=$3}}
  | NAT ELSTOK hexhex {ELSExpr {eid=$1; hexhex=$3}}
  | NAT EMTOK NAT NAT {EMExpr {eid1=$1; mptr = None; eid2=$4}}

recrule:
  | NAT RRTOK NAT NAT NAT {{rid = $1; ctorName=$3; numFields = $4; value = $5}}