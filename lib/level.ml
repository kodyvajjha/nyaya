(* Universe levels. *)

module Logger = Nyaya_parser.Util.MakeLogger (struct
  let header = "Level"
end)

module Fmt = CCFormat

type t =
  | Zero
  | Succ of t
    (* constructs a universe level that represents the larger of the left and right arguments. *)
  | Max of (t * t)
  (* represents the larger of the left and right arguments, unless the right argument simplifies to Zero, in which case the entire IMax resolves to 0. *)
  | IMax of (t * t)
  | Param of Name.t
[@@deriving show]

let param x = Param x

let of_string x = param (Name.of_string x)

let is_zero level =
  match level with
  | Zero -> true
  | _ -> false

let is_one level =
  match level with
  | Succ pred -> is_zero pred
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
    let l' = simplify l1 in
    let r' = simplify l2 in
    if is_zero l' || is_one l' then
      r'
    else (
      match r' with
      | Zero -> r'
      | Succ _ -> combining (l', r')
      | _ -> IMax (l', r')
    )

(** Collapses Succ layers into (base, offset). E.g., [Succ (Succ (Succ u))] will become [(u,3)]. Useful for pretty printing. *)
let rec peel_succ acc = function
  | Succ l -> peel_succ (acc + 1) l
  | l -> l, acc

let rec pp fpf t =
  match t with
  | Zero -> Fmt.fprintf fpf "0"
  | Succ l ->
    (* Count how many Succs in a row *)
    let base, offset = peel_succ 1 l in
    (match base with
    | Zero -> Fmt.fprintf fpf "%d" offset
    | _ -> Fmt.fprintf fpf "%a + %d" pp base offset)
  | Max (l1, l2) -> Fmt.fprintf fpf "max(%a,%a)" pp l1 pp l2
  | IMax (l1, l2) -> Fmt.fprintf fpf "imax(%a,%a)" pp l1 pp l2
  | Param l -> Fmt.fprintf fpf "%a" Name.pp l

(** Substitute [vs] for [ks] in [level]. *)
let rec subst ~(level : t) ~(ks : t list) ~(vs : t list) =
  match level with
  | Zero -> Zero
  | Succ l -> Succ (subst ~level:l ~ks ~vs)
  | Max (l1, l2) -> Max (subst ~level:l1 ~ks ~vs, subst ~level:l2 ~ks ~vs)
  | IMax (l1, l2) -> IMax (subst ~level:l1 ~ks ~vs, subst ~level:l2 ~ks ~vs)
  | Param _ ->
    let rec lookup ks vs =
      match ks, vs with
      | k :: ks', v :: vs' ->
        if level = k then
          v
        else
          lookup ks' vs'
      | _ -> level
    in
    lookup ks vs

let subst_simp ~level ~ks ~vs = subst ~level ~ks ~vs |> simplify

(** Returns a list of [levels] with [vs] substituted for [ks]. *)
let subst_levels ~(levels : t list) ~ks ~vs =
  CCList.map (fun level -> subst_simp ~level ~ks ~vs) levels

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
  | (Param _ | Zero), Max (a, b) -> leq x a balance || leq x b balance
  (* imax *)
  | IMax (a1, b1), IMax (a2, b2) when a1 == a2 && b1 == b2 -> true
  | IMax (_, Param p), _ -> cases x y p balance
  | _, IMax (_, Param p) -> cases x y p balance
  | IMax (a, IMax (b, c)), _ -> leq (Max (IMax (a, c), IMax (b, c))) y balance
  | IMax (a, Max (b, c)), _ ->
    leq (simplify (Max (IMax (a, b), IMax (a, c)))) y balance
  | _, IMax (a, IMax (b, c)) -> leq x (Max (IMax (a, c), IMax (b, c))) balance
  | _, IMax (a, Max (b, c)) ->
    leq x (simplify (Max (IMax (a, b), IMax (a, c)))) balance
  | _ ->
    Logger.err "leq : not defined for that case where x,y = (%a,%a)!"
      (Failure "leq") pp x pp y

and cases l1 l2 p balance =
  leq
    (simplify (subst ~level:l1 ~ks:[ Param p ] ~vs:[ Zero ]))
    (simplify (subst ~level:l2 ~ks:[ Param p ] ~vs:[ Zero ]))
    balance
  && leq
       (simplify (subst ~level:l1 ~ks:[ Param p ] ~vs:[ Succ (Param p) ]))
       (simplify (subst ~level:l2 ~ks:[ Param p ] ~vs:[ Succ (Param p) ]))
       balance

(** Partial order on levels. *)
let ( <= ) l1 l2 = leq l1 l2 0

let eq l1 l2 = l1 <= l2 && l2 <= l1

let ( === ) = eq

open Nyaya_parser

let table name_table (ast : Ast.t) : (Ast.uidx, t) Hashtbl.t =
  let resolved_table : (Ast.uidx, t) Hashtbl.t =
    Hashtbl.create (CCList.length ast.items)
  in
  Hashtbl.add resolved_table 0 Zero;
  let rec resolve uid =
    match Hashtbl.find_opt resolved_table uid with
    | Some l -> l
    | None when uid = 0 -> Zero
    | None ->
      let lvl = Hashtbl.find (Ast.Hashed.levels ast) uid in
      let resolved_level =
        match lvl with
        | Ast.Level.USLevel { uid2; _ } -> Succ (resolve uid2)
        | Ast.Level.UMLevel { uid2; uid3; _ } -> Max (resolve uid2, resolve uid3)
        | Ast.Level.UIMLevel { uid2; uid3; _ } ->
          IMax (resolve uid2, resolve uid3)
        | Ast.Level.UPLevel { nid; _ } ->
          (match CCHashtbl.get name_table nid with
          | None -> failwith "level_resolver: could not find binding"
          | Some name -> Param name)
      in
      Hashtbl.add resolved_table uid resolved_level;
      resolved_level
  in
  Hashtbl.iter (fun uid _ -> ignore (resolve uid)) (Ast.Hashed.levels ast);
  Logger.info "Finished resolving levels. Total number : %d."
    (Hashtbl.length (Ast.Hashed.levels ast));
  resolved_table
