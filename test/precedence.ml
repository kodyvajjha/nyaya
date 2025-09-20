module Fmt = CCFormat
open Nyaya

let expect ~msg ~got ~want =
  if String.equal got want then
    ()
  else
    Fmt.eprintf "FAIL: %s\nGOT : %S\nWANT: %S\n@." msg got want

let c s = Expr.const (Name.of_string s)

let lam s = Expr.lambda (Name.of_string s)

module Tests = struct
  open Expr

  let t_app_flat = mk_app (c "f") [ c "a"; c "b"; c "c" ]

  let t_fun_fnpos = mk_app (lam "x" (c "A") (bv 0)) [ c "a" ]

  let t_fun_arg = mk_app (c "f") [ lam "x" (c "A") (bv 0) ]

  let t_forall_arg = mk_app (c "f") [ pi (Name.of_string "x") (c "A") (c "A") ]

  let t_let_arg =
    mk_app (c "f") [ letin (Name.of_string "x") (c "A") (c "a") (bv 0) ]

  let t_app_in_app =
    mk_app (mk_app (c "f") [ c "a" ]) [ mk_app (c "g") [ c "b" ] ]

  let t_forall_chain =
    pi (Name.of_string "x") (c "A") (pi (Name.of_string "y") (c "B") (c "A"))

  let t_lam_chain =
    lam "α" (sort (Succ (Level.of_string "u"))) (lam "x" (bv 0) (bv 0))

  let t_proj_app =
    mk_app (Proj { name = Name.of_string "f"; nat = 0; expr = c "p" }) [ c "a" ]

  let t_app_then_proj =
    Proj { name = Name.of_string "f"; nat = 0; expr = mk_app (c "f") [ c "a" ] }

  let t_sort_arg =
    mk_app
      (const
         ~ups:[ Level.of_string "u"; Level.of_string "v" ]
         (Name.of_string "Foo"))
      [ sort (Level.Max (Level.of_string "u1", Level.of_string "v1")) ]
end

let () =
  Fmt.printf "=== Precedence tests ===@.";
  (* Keep output narrow to force line breaks during dev if desired *)
  Fmt.pp_set_margin Fmt.std_formatter 60;

  (* 1) App left-assoc: f a b c *)
  expect ~msg:"flat app" ~got:(Expr.to_string Tests.t_app_flat) ~want:"f a b c";

  (* 2) Parens for binder-in-function-position: (fun x => x) a *)
  expect ~msg:"lambda as function needs parens"
    ~got:(Expr.to_string Tests.t_fun_fnpos)
    ~want:"fun (x : A) => #0 a";

  (* 3) Parens for binder-as-argument: f (fun x => x) *)
  expect ~msg:"lambda as arg needs parens"
    ~got:(Expr.to_string Tests.t_fun_arg)
    ~want:"f (fun (x : A) => #0)";

  (* 4) Parens for ∀ as arg: f (forall (x : A), A) *)
  expect ~msg:"forall as arg needs parens"
    ~got:(Expr.to_string Tests.t_forall_arg)
    ~want:"f (forall (x : A), A)";

  (* 5) Parens for let as arg: f (let x : A := a in x) *)
  expect ~msg:"let as arg needs parens"
    ~got:(Expr.to_string Tests.t_let_arg)
    ~want:"f (let x : A := a in #0)";
  (* 6) Nested app vs app precedence: (f a) (g b) -> prints “(f a) (g b)” (head/arg both apps) *)
  expect ~msg:"app in function and argument positions"
    ~got:(Expr.to_string Tests.t_app_in_app)
    ~want:"f a (g b)";

  (* 7) Forall chains: ∀ (x : A) (y : B), A *)
  expect ~msg:"forall chain"
    ~got:(Expr.to_string Tests.t_forall_chain)
    ~want:"forall (x : A)(y : B), A";

  (* 8) Lam chain: fun (α : Sort u+1) (x : #0) => #0 *)
  expect ~msg:"lambda chain"
    ~got:(Expr.to_string Tests.t_lam_chain)
    ~want:"fun (α : Sort u + 1)(x : #0) => #0";
  (* 9) Proj vs app (if you have projections): (p.f.0) a  — proj binds tighter than app *)
  expect ~msg:"projection then app"
    ~got:(Expr.to_string Tests.t_proj_app)
    ~want:"p.f.0 a";

  (* 10) App then proj needs parens: (f a).f.0 *)
  expect ~msg:"app then projection needs parens"
    ~got:(Expr.to_string Tests.t_app_then_proj)
    ~want:"(f a).f.0";

  (* 11) Sort with complex level as arg should generally be parenthesized if your printer treats Sort … as non-atom; adjust expected if Sort is atom in your precedence *)
  expect ~msg:"Sort with complex level as argument"
    ~got:(Expr.to_string Tests.t_sort_arg)
    ~want:"Foo.{u,v} (Sort max(u1,v1))"
