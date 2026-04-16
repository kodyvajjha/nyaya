(* Per-session monotonic identifier source.

   Each checker context owns one of these for free-variable ids.
   Instance-based rather than a global counter so independent checking
   sessions do not share state. *)

type t = { mutable next : int }

let create () = { next = 0 }

let next t =
  let id = t.next in
  t.next <- id + 1;
  id
