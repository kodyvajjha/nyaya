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
