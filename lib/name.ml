(* This module contains the first of the Lean kernel's primitive types; the
   type of names. It provides kernel items with a way of addressing things. *)

type t =
  | Anon
  | Str of (t * string)
  | Num of (t * int)
[@@deriving show]
