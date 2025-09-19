(* ---------------- Programmatic “stress” cases ---------------- *)
module E = Nyaya.Expr
module N = Nyaya.Name
module L = Nyaya.Level
module Fmt = CCFormat

let c s = E.const (N.of_string s)

let lam s = E.lambda (N.of_string s)

let rec gen depth : E.t =
  let open E in
  let atom () =
    match Random.int 4 with
    | 0 -> c "A"
    | 1 -> c "B"
    | 2 -> sort (L.of_string "u1")
    | _ -> BoundVar (Random.int 2)
  in
  if depth <= 0 then
    atom ()
  else (
    match Random.int 6 with
    | 0 -> mk_app (gen (depth - 1)) [ gen (depth - 1) ] (* App *)
    | 1 -> lam "x" (gen (depth - 1)) (gen (depth - 1)) (* Lam *)
    | 2 -> pi (N.of_string "x") (gen (depth - 1)) (gen (depth - 1))
    (* Forall *)
    | 3 ->
      letin (N.of_string "y")
        (gen (depth - 1))
        (gen (depth - 1))
        (gen (depth - 1))
      (* Let *)
    | 4 ->
      Proj
        { name = N.of_string "f"; nat = Random.int 2; expr = gen (depth - 1) }
      (* Proj *)
    | _ -> atom ()
  )

let needs_parens_as_arg (e : E.t) =
  (* Heuristic: if e is Lam/Forall/Let/App with its own children, then as an argument to App it should be parenthesized. *)
  match e with
  | E.Lam _ | E.Forall _ | E.Let _ -> true
  | E.App _ ->
    (* app-as-arg often does not need parens (since app has same precedence),
       but if your associativity policy requires, toggle this. *)
    false
  | _ -> false

(* Generate N random (f e) and check that when needs_parens_as_arg e,
   the printed string contains "(fun", "(∀", or "(let" accordingly. *)
let check_paren_arg e =
  let term = E.mk_app (c "f") [ e ] in
  let s = E.to_string term in
  if needs_parens_as_arg e then (
    let ok =
      String.contains s '('
      (* cheap check; refine by searching specific tokens if you prefer *)
    in
    if not ok then (
      Fmt.eprintf "Paren check failed for argument:\n  %s\n@." s;
      exit 1
    )
  )

let () =
  for _i = 1 to 100 do
    let rand = gen 3 in
    Fmt.printf "random : %s@." (E.to_string rand);
    check_paren_arg (gen 3)
  done;
  Fmt.printf "Random paren stress passed ✅@."
