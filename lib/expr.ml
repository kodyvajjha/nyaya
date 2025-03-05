(* TODO: Expressions need to store some data inline or cache it somewhere to prevent prohibitively expensive recomputation. 
Perhaps we could do hashconsing.*)

type binfo =
  | Default
  | Implicit
  | InstanceImplicit
  | StrictImplicit
[@@deriving show]

type literal =
  | NatLit of Z.t [@printer Z.pp_print]
  | StrLit of string
[@@deriving show]

type t =
  | BoundVar of int
  | FreeVar of {
      name: Name.t;
      expr: t;
      info: binfo;
      fvarId: int; (* de Bruijn indices*)
    }
  | Const of {
      name: Name.t;
      uparams: Level.t list;
    }
  | Sort of Level.t
  | App of t * t

(**   binderName      body
      |            |
fun (foo : Bar) => 0 
            |         
        binderType     *)
  | Lam of {
      name: Name.t;
      btype: t;
      (* binderInfo is reflected by the style of brackets used 
      to surround the binder*)
      binfo: binfo; 
      body: t;
    }
  | Forall of {
      name: Name.t;
      btype: t;
      binfo: binfo;
      body: t;
    }
      (** 
      binderName   val
      |            |
let (foo : Bar) := 0; foo
            |          |
        binderType     .... body
  *)
  | Let of {
      name: Name.t;
      btype: t;
      value: t;
      body: t;
    }
  (* Structure projections. *)
  | Proj of {
      name: Name.t;
      nat: int;
      expr: t;
    }
  | Literal of literal
[@@deriving show]
