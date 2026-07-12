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

## 1. Skip defeq check for Prop-typed parameters in `infer` (App case) — RESOLVED 2026-07-12

**Resolved.** The App case no longer skips the argument type-check for
Prop-typed parameters; it always runs `isDefEq env btype arg_type`, matching the
kernel's `infer_app`. Proof irrelevance applies only inside `isDefEq`, never as a
license to skip inference. Removing the skip rejects the two arena cases it had
been masking (`bad/proj-of-prop` and `bad/nat-rec-rules`) and required making Nat
reduction kernel-faithful: removing the `is_nat_builtin` delta-guard and fixing
three argument-forcing bugs in `nat_lit_reduce`/`isDefEq`. Full write-up in
`doc/changelog.md` (2026-07-12 entry). The only residue is
`good/perf/grind-ring-5`, which now **false-rejects** (hits the `whnf` depth
limit on a deep symbolic `Nat.casesOn`/`Nat.below` reduction where the kernel
stays shallow) — a reduction *completeness* gap, not a soundness one: the change
trades a former false-*accept* for a surfaced false-*reject*.

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
