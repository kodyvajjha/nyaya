(* Universe levels. *)

type t =
  | Zero
  | Succ of t
    (* constructs a universe level that represents the larger of the left and right arguments. *)
  | Max of (t * t)
  (* represents the larger of the left and right arguments, unless the right argument simplifies to Zero, in which case the entire IMax resolves to 0. *)
  | IMax of (t * t)
  | Param of Name.t
[@@deriving show]

let is_zero level =
  match level with
  | Zero -> true
  | _ -> false

let is_any_max level =
  match level with
  | Max _ -> true
  | IMax _ -> true
  | _ -> false

let is_param level =
  match level with
  | Param _ -> true
  | _ -> false

let rec combining (l, r) =
  match l, r with
  | Zero, _ -> r
  | _, Zero -> l
  | Succ l1, Succ l2 -> Succ (combining (l1, l2))
  | _ -> Max (l, r)

let rec simplify (level : t) : t =
  match (level : t) with
  | Zero | Param _ -> level
  | Succ l -> Succ (simplify l)
  | Max (l1, l2) -> combining (simplify l1, simplify l2)
  | IMax (l1, l2) ->
    (match simplify l2 with
    | Succ _ as l2_ -> combining (simplify l1, l2_)
    | Zero -> Zero
    | l2_ -> IMax (simplify l1, l2_))

(* TODO: Maybe replace this with a Hashtbl.  *)
let rec subst (level : t) p q =
  match level with
  | Zero -> Zero
  | Succ l -> Succ (subst l p q)
  | Max (l1, l2) -> Max (subst l1 p q, subst l2 p q)
  | IMax (l1, l2) -> IMax (subst l1 p q, subst l2 p q)
  | Param _name -> assert false (* NYI *)

let rec leq (x : t) (y : t) (balance : int) : bool =
  match x, y with
  | Zero, _ when balance >= 0 -> true
  | _, Zero when balance < 0 -> false
  | Param i, Param j -> i == j && balance >= 0
  | Param _, Zero -> false
  | Zero, Param _ -> balance >= 0
  | Succ x1, _ -> leq x1 y (balance - 1)
  | _, Succ y1 -> leq x y1 (balance + 1)
  (* descend left *)
  | Max (a, b), _ -> leq a y balance && leq b y balance
  (* descend right *)
  | (Param _ | Zero), Max (a, b) -> leq x a balance || leq y b balance
  (* imax *)
  | IMax (a1, b1), IMax (a2, b2) when a1 == a2 && b1 == b2 -> true
  | IMax (_, (Param _ as _p)), _ -> assert false (* NYI*)
  | _, IMax (_, (Param _ as _p)) -> assert false (* NYI*)
  | IMax (a, IMax (b, c)), _ -> leq (Max (IMax (a, c), IMax (b, c))) y balance
  | IMax (a, Max (b, c)), _ ->
    leq (simplify (Max (IMax (a, b), IMax (a, c)))) y balance
  | _, IMax (a, IMax (b, c)) -> leq x (Max (IMax (a, c), IMax (b, c))) balance
  | _, IMax (a, Max (b, c)) ->
    leq x (simplify (Max (IMax (a, b), IMax (a, c)))) balance
  | _ ->
    let err =
      CCFormat.sprintf "leq : not defined for that case where x,y = (%a,%a)!" pp
        x pp y
    in
    failwith err
