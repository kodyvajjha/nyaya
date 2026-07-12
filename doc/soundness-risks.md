# nyaya soundness risks

Shortcuts in `lib/tc.ml` that make more terms pass than a strict reading of
the Lean 4 kernel would allow. Nothing here is committed lightly: each entry
existed because it unblocked real `init.export` declarations, but the exact
generalization applied is broader than the kernel rule it's based on, and
that gap hasn't been closed yet.

This file exists because the corpus we test against (`init.export`, all
valid Lean `Init` declarations) cannot detect unsoundness by construction —
an overly permissive whnf/isDefEq rule only ever makes *more* valid
declarations pass, never fewer. Catching this needs a corpus of *invalid*
declarations a sound kernel should reject, which is deferred to the
mathlib/Lean-arena stage. Until then, this file is the audit trail: anything
listed here needs scrutiny (or a negative test) before nyaya's results can
be trusted as a soundness reference, not just a "does it agree with Lean on
valid input" check.

---

## 1. Skip defeq check for Prop-typed parameters in `infer` (App case)

**File:** `lib/tc.ml`, `infer`, `Expr.App` case (~line 742-763)

**What it does:** When inferring the type of `f arg` and the callee's
parameter type (`btype`) is a `Prop` (`Sort 0`), the check
`isDefEq env btype arg_type` is skipped entirely — `arg`'s type is never
even inferred. For any other (non-Prop) parameter type, the normal defeq
check runs.

**Why it was added (2026-03-27 changelog entry, "Skip defeq check for
Prop-typed arguments in infer App"):** auto-generated proof terms
(`omega`, `decide`, etc.) can be enormous, and normalizing them to check
defeq is expensive but pointless — proof irrelevance means any two proofs
of the same Prop are interchangeable, so the exact proof term structurally
never needs to be compared.

**Why it's broader than the real kernel rule:** the Lean kernel's actual
proof irrelevance (correctly implemented elsewhere in this file — see
`isDefEq_impl`, ~line 1002-1017 — `is_prop s && is_prop t && isDefEq env s
t`) requires inferring *both* terms' types, confirming *both* are Props,
and confirming those two Prop types are themselves definitionally equal,
before declaring the two proofs equal. The App-case shortcut does none of
that: it never infers `arg`'s type and never compares it to `btype` at
all. It only checks that the *expected* (parameter) type is a Prop —
not that the argument's actual type is a Prop, let alone the *same*
(defeq) Prop as the parameter.

**Concrete failure shape:** `f a` where `f : P -> T` (`P : Prop`) and `a`
has some type `Q` with `Q` not definitionally equal to `P` — including,
worst case, `Q` not even being a Prop — would currently be accepted by
`infer`'s App case, because the check that would have caught this
(`isDefEq env btype arg_type`) is skipped before it ever runs.

**What a correctly-scoped version needs:** infer `arg`'s type, confirm
*it* is also a Prop (not just that `btype` is), and only then skip the
*structural* proof comparison — i.e. apply the same two-Props-defeq
check `isDefEq_impl` already does correctly, rather than skipping the
domain/argument type match altogether.

**Status:** unresolved. Not touched by the autoloop unless a fix is
proposed with its own kernel citation and lands as its own entry here
(narrowing it) or in `doc/changelog.md` (replacing it with the correctly
scoped version).

**2026-07-12 — arena `bad/proj-of-prop` and the coupling to a `whnf`
incompleteness (discovered while attempting the fix):** This shortcut is
*directly* the reason nyaya wrongly **accepts** `bad/proj-of-prop`
(expected reject): it builds `Wrapper.mk True.intro` where
`Wrapper.mk : False → Wrapper`, and because the binder type `False` is a
Prop, the argument `True.intro : True` is never checked against it, so the
subsequent `.p` projection yields a proof of `False`. The kernel-faithful
fix is exactly what this entry's "correctly-scoped version" says and what
`type_checker.cpp`'s `infer_app` does: infer the argument's type and require
`is_def_eq(a_type, d_type)` for *every* argument, with no Prop special-case.
Implemented naively (always check), it correctly rejects `proj-of-prop`
(`True` ≢ `False`) — **but it regresses the good perf case
`good/perf/grind-ring-5`**, which then falsely rejects at `Nat.div_eq`. The
failing obligation there is `is_def_eq` between `Eq Nat (Nat.div x y) (ite …)`
(the binder type) and `Eq Nat (dite (0<y) … ) (ite …)` (the argument's
inferred type): defeq only if `Nat.div x y` unfolds to its `dite` form on
*symbolic* `x`, `y`. nyaya deliberately guards `Nat.div`/other Nat builtins
against delta-unfolding on symbolic arguments (to avoid non-termination — see
`doc/changelog.md` and the memory note on that guard), so its `whnf`/`isDefEq`
cannot discharge `Nat.div_eq` at all. `grind-ring-5` was therefore passing
*only because* this unsound Prop-skip hid that incompleteness.

So removing this unsoundness is blocked behind a real `whnf`/`isDefEq`
completeness improvement: reduce a guarded Nat builtin like `Nat.div` by
exactly one definitional (equation-lemma) step when needed, without
reintroducing the non-termination the guard exists to prevent. That is
architectural, adjacent to a known hazard, and out of scope for a localized
loop fix — the autoloop reverted the naive change (no regression left) and
deferred `proj-of-prop` behind this item rather than either shipping an
unsound accept or a good-case regression.

**2026-07-12 (second attempt — the coupling is broader than "one `Nat.div`
step"):** Removing the Prop-skip and requiring `is_def_eq(arg_type, btype)` for
every argument does correctly reject `proj-of-prop` (and incidentally
`bad/nat-rec-rules`). But it regresses **two** valid files, not just
`grind-ring-5`: `good/init-prelude` also fails, at `Nat.modCoreGo_lt`. Both
regressions are in the same cluster — the fuel-recursive definitions of
`Nat.div` / `Nat.modCore`.

Concretely: `Nat.div n m` unfolds to a helper `Nat.div.go y hy fuel x h` (fuel
is the 3rd argument; `go` recurses structurally on `fuel`), and likewise
`Nat.modCore.go`. Their own soundness lemmas (`Nat.div.go.fuel_congr`,
`Nat.modCoreGo_lt`, …) contain `Prop`-typed obligations that the App-case skip
was hiding wholesale. Verifying them needs nyaya to (a) reduce `go` on a
symbolic dividend/divisor and get *stuck at the manifest `dite`* rather than
either blowing up or refusing to reduce, and (b) discharge a `dite`
vs `Decidable.casesOn` congruence between the two sides. Attempted mitigations
that do **not** suffice on their own:

- Restoring one delta step of the guarded `Nat.div` (so `Nat.div x y` exposes
  its `dite`) — necessary but not sufficient.
- A *fuel-aware* guard on `Nat.div.go`/`Nat.modCore.go` (leave them unreduced
  only while `fuel` is symbolic, still reduce on a literal/`succ` fuel) — this
  removes the blowup and makes `grind-ring-5` fast again, but the proofs still
  fail to type-check because the residual `dite`/`Decidable.casesOn` congruence
  is not discharged.

So the honest scope is a `whnf`/`isDefEq` completeness pass over the whole
fuel-recursive `Nat.div`/`Nat.modCore` reduction (helper reduction + the
`dite`/`Decidable.casesOn` congruence it bottoms out in), verified against
`init-prelude` and `grind-ring-5`, not a single equation-lemma step. Reverted
again; `proj-of-prop` stays deferred behind this item.

**2026-07-12 (third attempt — the *guard*, not just the Prop-skip, is the
masker; kernel-faithful direction confirmed):** Citations settle the design.
The official kernel special-cases Nat reduction *only for literals*
(`reduce_bin_nat_op` bails with `none_expr()` unless both args are
`is_nat_lit_ext`), and for symbolic args just runs general `whnf`, whose
`reduce_recursor` returns `none` (stuck) when the major premise is not a
constructor. So the faithful fix is **not** to add a fuel-aware guard + a `dite`
congruence rule (both are un-kernel-like workarounds) — it is to **remove the
`is_nat_builtin` delta-guard entirely** (keep only the literal fast path
`nat_lit_reduce`, = `reduce_nat`) and let general reduction get stuck on its own,
exactly as the kernel does.

Ran that experiment (Prop-skip removed **and** guard disabled at both call sites
in `whnf_impl`). Result on the full corpus — `4 red`, better than the clean
baseline's `5`:
- `bad/proj-of-prop` → now correctly **rejects** (the #1 fix).
- `bad/nat-rec-rules` → now correctly **rejects** too (its #2 obligation was also
  Prop-typed and masked by the same skip).
- `good/init-prelude` → **accepts** in ~22s. The memory note's feared "O(n)
  blowup on huge literals" from removing the guard **did not materialise** — no
  file in the corpus OOMs or slows meaningfully; `nat_lit_reduce` already covers
  the literal cases the guard was protecting.
- `good/perf/grind-ring-5` → the **sole** regression, and it is no longer an OOM:
  it trips the `whnf` depth limit (`NYAYA_WHNF_MAX_DEPTH=2000`) at
  `Nat.casesOn` depth 2001. Raising the limit to 100000 did *not* let it finish
  in >3 min, so a symbolic Nat reduction is going unboundedly deep (or enormously
  large) where the real kernel stays shallow — i.e. a genuine `whnf` stuck-ness
  gap that the guard was masking, now isolated to this one perf case.

Net: removing guard + Prop-skip is the correct, citation-backed direction and
clears two soundness bugs at once, with no huge-literal fallout. The remaining
work is a single reduction-completeness fix (make that symbolic Nat recursion
get stuck like the kernel's `reduce_recursor`, or compute it natively), verified
against `grind-ring-5`. Reverted to clean pending that fix — the tree cannot
carry a valid-file regression even though the net red count improves.

---

## 2. Exported recursor reduction rules are trusted, not reconstructed

**File:** `lib/tc.ml`, `iota_at_head` (`Decl.Rec { rules; ... }` branch, ~line
202) — and, upstream, environment construction, which stores the export's
`rules` verbatim without validating them.

**What it does:** For iota reduction of `C.rec … (ctor …)`, nyaya looks up the
recursor declaration and uses the `rhs` of the exported rule for that
constructor *as given*. It never checks that `rhs` is the canonical reduction
rule the inductive definition implies — it trusts the exporter.

**Why this is unsound:** the reduction rules are precisely the computational
content of an inductive type; a wrong rule lets `C.rec` compute anything. The
arena case `test/bad/nat-rec-rules.ndjson` (expected **reject**) exploits this
directly: it ships a `Nat` whose `Nat.rec` successor rule has
`rhs = λ motive h_zero h_succ n. h_zero` — i.e. `Nat.rec … (succ n)` reduces to
the **zero**-case minor premise instead of `h_succ n (Nat.rec … n)`. Combined
with a `Nat.beq`-style definition that delegates to this recursor for symbolic
arguments, this derives `False`, and nyaya currently **accepts it (exit 0)**
because it reduces with the malformed exported rule. (The correct succ rule
returns a term of type `motive (succ n)`; the malformed one returns
`h_zero : motive zero`, so it is not even type-correct as a rule — but nyaya
never checks that.)

**What a correctly-scoped version needs:** the kernel must **independently
construct** each recursor's reduction rules from the inductive declaration (the
`mk_rec` computation in `src/kernel/inductive/inductive.cpp`) and either use
those in place of the exported `rhs`, or validate the exported rules against
them. This is the fix the arena's own reference points at (nanoda commit
`1283899`, "the kernel independently constructs recursor rules and properly
validates them against exported declarations"). This is genuine architectural
work (recursor-rule reconstruction), not a localized patch — flagged in
`doc/arena-correctness-loop-plan.md` as a stop-and-surface condition. The
autoloop stopped here rather than half-building it.

**2026-07-12 — recursor naming/generation (`bad/130`, `bad/131`):** Because
nyaya reads recursors from the export instead of generating them, it also can't
catch a *misnamed* or *duplicated* recursor. `130_misnamed_rec_user` ships the
recursor as `misnamed_rec.not_rec` (a real kernel recursor is always generated
as `<inductive>.rec`) and a def that uses it; `131_dup_rec_def2` additionally
puts a plain `def` on the reserved `dup_rec_def2.rec` name. A sound kernel
generates `<inductive>.rec` and rejects the clash / the missing-canonical
recursor. A pure name-suffix heuristic ("a `Decl.Rec` must be named `.rec`")
would catch both but has no kernel citation (the kernel *generates* the name,
never *checks* an incoming one), so it is RISKY and was not committed. These two
belong with this recursor-reconstruction item.

**Status:** unresolved; open architectural item. This is a *detected*
unsoundness — `dune build @runtest` shows `bad/nat-rec-rules`, `bad/130`,
`bad/131` red — unlike entry 1, which the current corpus does not catch.

---

## 3. Strict positivity is not checked for *nested* inductives

**File:** `lib/tc.ml`, `check_ctor`.

**Update (2026-07-12):** `check_ctor` now checks strict positivity of every
constructor field (`check_positivity`): the inductive may not occur to the left
of an arrow, and may otherwise occur only as a plain recursive application. That
cleared the detected cases `bad/tutorial/051_indNeg`, `054_indNegReducible`, and
`111_reflOccLeft` (see `doc/changelog.md`). Together with the earlier
header/universe checks, the whole detected 047–058/111 cluster is now rejected.

**Remaining gap — nested inductives.** The positivity check is gated on
`num_nested = 0`. The real kernel un-nests nested inductives into fresh
parameters *before* checking positivity; nyaya does not un-nest, so running the
check on a nested field (e.g. `List I`) would false-reject it — the recursive
occurrence sits inside `List` rather than being a bare `I params indices`. The
export's `num_nested` count is the kernel's own signal for whether un-nesting is
needed, so positivity runs only when it is zero. **Consequently a nested
inductive skips positivity entirely and an unsound nested occurrence would be
accepted.**

**Why this is (still) unsound:** a nested inductive can encode a non-positive
occurrence through the nesting type, and nyaya would not catch it. No arena case
currently exercises this (the corpus's positivity cases are all `num_nested =
0`), so unlike before, this gap is **undetected** by `dune build @runtest`.

**What a correctly-scoped version needs:** the nested-inductive un-nesting the
real kernel performs before `check_positivity` (specialising each nesting
container into an auxiliary inductive), after which the existing positivity
recursion would apply directly.

**Status:** unresolved; open architectural item, **undetected** (no red test).
Narrowed from the original 047–058 cluster to nested-inductive positivity only.
