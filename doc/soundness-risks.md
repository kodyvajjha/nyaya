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

## 3. Constructor strict positivity (`check_positivity`) is not done

**File:** `lib/tc.ml`, `check_ctor` (~line 1157).

**Update (2026-07-12):** `check_ctor` no longer stubs everything to `true`. The
header-consistency and universe obligations are now a faithful port of Lean's
`check_constructors` — parameter binders def-eq the inductive's, field universes
`≤` the inductive's (or `Prop`), and the manifest result is `is_valid_ind_app`
(correct head const with its universe params, correct parameter fvars, no
inductive occurrence in the result indices). That cleared the header/universe
half of the old cluster: `bad/tutorial/047, 048, 049, 050, 053, 058` (see
`doc/changelog.md`). The remaining gap is **strict positivity only**.

**Why this is unsound:** a constructor field with a non-positive occurrence of
the inductive (the inductive to the left of an arrow) lets you derive `False`.
Two arena cases (both expected **reject**, both currently **accepted**) exercise
exactly this:
- `051_indNeg` — the classic negative occurrence: a field of type
  `(I → I) → I`, with `I` to the left of the inner arrow.
- `054_indNegReducible` — a negative occurrence nested inside a head-normal form
  that *could* reduce it away (`constType aType I → I`); the kernel treats it as
  negative anyway, without reducing past hnf.

**Why it was NOT ported with the rest:** the kernel's `check_positivity` runs
*after* `add_inductive_fn` un-nests nested inductives into fresh auxiliary
parameters. nyaya reads inductives straight from the export and does **not**
un-nest, so a naive port of `check_positivity` — which only accepts recursive
occurrences that are literally `I params indices` via `is_valid_ind_app` — would
false-reject the valid nested inductives in the good corpus (e.g. a field of
type `List (Tree a)`). Positivity is the plan's named stop-and-surface
architectural item, and it is coupled to the missing nested-inductive support.

**What a correctly-scoped version needs:** the nested-inductive un-nesting the
real kernel performs before `check_positivity`, then the positivity recursion
itself (`check_positivity` in `src/kernel/inductive.cpp`: whnf to hnf; if a
`pi`, the domain must have no inductive occurrence and recurse into the body;
else it must be a valid recursive application).

**Status:** unresolved; open architectural item, detected (`bad/tutorial/051`,
`054` red). Narrowed from the original 047–058 cluster to positivity only.
