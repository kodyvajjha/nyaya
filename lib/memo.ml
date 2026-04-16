(* Typed memoisation table.

   A thin wrapper over [Hashtbl] exposing only the operations the
   checker needs. Keeping the surface small makes memo usage obvious
   at call sites and leaves room to swap the concrete representation
   (e.g. bounded cache, ephemeron) later without churn. *)

type ('k, 'v) t = ('k, 'v) Hashtbl.t

let create n : ('k, 'v) t = Hashtbl.create n

let find_opt = Hashtbl.find_opt

let add = Hashtbl.replace

let clear = Hashtbl.reset
