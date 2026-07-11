# Provenance: logic in `tc.ml` derived from the C++ kernel

`lib/tc.ml`'s inline comments used to carry long justifications, many of
them citing or quoting `lean4/src/kernel/type_checker.cpp` (and, once,
`inductive.h`) directly. Those comments have been trimmed to one line each;
this file is where the provenance detail moved instead.

The reason this list exists as its own document: nyaya's only usable test
corpus (`init.export`) is a set of terms a *sound* kernel accepts, so it can
only catch nyaya being too strict, never too permissive. Copying reasoning
or code shape from the real kernel's C++ implementation is a fast way to get
something that passes that corpus, but it also means any bug in that C++
(there is no reason to assume it has none) could get imported and would be
invisible to our own testing. Every entry below is a candidate for
independent re-derivation from the type theory rather than from the C++, and
for a negative-test corpus that could actually catch a mistake here (see
also `doc/soundness-risks.md`, a related but distinct list of known
over-generalizations).

---

## 1. `Reduce.string_lit_to_ctor`
Unicode code-point decoding and the `String.mk (List.cons ...)` shape it
builds mirror the kernel's `string_lit_to_constructor`.

## 2. `iota_at_head`, K-like reduction (`is_K` branch)
Replacing a stuck major premise with its type's nullary constructor (guarded
by a defeq check) is modeled on `inductive.h`'s `to_cnstr_when_K`. Not
independently re-derived from why K-reduction is sound for subsingleton
eliminators (`Eq`, `HEq`, ...).

## 3. `iota_at_head`, `Decl.Quot` branch (`Quot.ind`/`Quot.lift`)
The fixed spine-index positions (arg 3 = function/motive, arg 4 = major for
`Quot.ind`, arg 5 = major for `Quot.lift`) and the "reduce to `f a` with
`a` = `Quot.mk`'s third argument, then re-apply the suffix" shape come from
the kernel's `reduce_quot_rec` (`kernel/quot.cpp`), reproduced from training
knowledge of that source, not independently derived or fetched fresh in this
session.

## 4. `Reduce.nat_lit_reduce`, overall approach
The general strategy (recognize Nat literal builtins and compute directly
rather than delta-unfolding into recursive definitions) follows "Type
Checking in Lean 4" §3.5 (Literals) and the kernel's `reduce_nat` dispatch
table, not just the Lean-level `Nat.add`/`Nat.mul`/etc. definitions.

## 5. `nat_lit_reduce`, division/modulo-by-zero conventions
`Nat.div n 0 = 0` and `Nat.mod n 0 = n` are asserted as "kernel convention"
in a couple of places. These happen to also follow from the actual
`Nat.div`/`Nat.mod` Lean definitions (`Init/Prelude.lean`), so this one is
lower-risk than the others here — but the "kernel convention" framing was
never independently checked against `reduce_nat` itself, only assumed.

## 6. `is_nat_builtin_name`
The decision of exactly which Nat operations must never be delta-unfolded
(`add/sub/mul/pow/div/mod/beq/ble/xor/bitwise`, but pointedly *not*
`land/lor/shiftLeft/shiftRight/testBit`) is justified by "the real kernel's
`type_checker.cpp` reduces these natively via GMP and never delta-unfolds
`Nat.bitwise`" — a direct claim about the C++ `reduce_nat` dispatch table's
contents, not verified against nyaya's own semantics independently.

Why `land`/`lor`/`shiftLeft`/`shiftRight`/`testBit` are left *unguarded*
despite the same O(n)-blowup argument applying to them too (this part is
homegrown, not kernel-derived): guarding a name blocks *all* delta on it,
including the single unconditional step some already-passing equation
lemma needs (`Nat.lor.eq_1`, `Nat.land.eq_1`, `Nat.shiftLeft.eq_1/eq_2`).
`Nat.xor.eq_1` has the identical one-step need, which is why guarding
`Nat.xor` required adding a matching one-step carve-out in
`nat_lit_reduce`'s own `Nat.xor` case rather than leaving it broken; no
such carve-out exists yet for the other four, so they stay unguarded until
a concrete declaration forces the same fix.

## 7. `infer_impl`, typing-rule pseudocode comments
The pseudocode blocks for `Lam`, `Forall`/`Pi`, `Const`, `App`, `Let`, and
`Proj` (e.g. `infer App(f, arg): match (whnf $ infer f) with | Pi ... `)
are transcriptions of "Type Checking in Lean 4"'s presentation of the
algorithm, not independently derived typing rules.

## 8. `isDefEq_impl`, `same_head_args_shortcut`
Directly modeled on, and originally included a verbatim quote of,
`type_checker.cpp`'s `is_def_eq_app` and the same-head optimization inside
`lazy_delta_reduction_step`. The full C++ this was based on:

```cpp
bool type_checker::is_def_eq_app(expr const & t, expr const & s) {
    if (is_app(t) && is_app(s)) {
        buffer<expr> t_args, s_args;
        expr t_fn = get_app_args(t, t_args);
        expr s_fn = get_app_args(s, s_args);
        if (is_def_eq(t_fn, s_fn) && t_args.size() == s_args.size()) {
            unsigned i = 0;
            for (; i < t_args.size(); i++)
                if (!is_def_eq(t_args[i], s_args[i])) break;
            if (i == t_args.size()) return true;
        }
    }
    return false;
}
```
plus, from `lazy_delta_reduction_step`, the gate that tries this before
unfolding when both heads are the same regular-hint declaration with equal
universe params.

## 9. `isDefEq_impl`, `App, App` branch sequencing
The order "try per-argument congruence first (as a plain bool, catching any
exception from a nested comparison), then struct-eta on the whole terms,
only then fail" mirrors `is_def_eq_core`'s actual sequence: `is_def_eq_app`
→ `try_eta_expansion` → `try_eta_struct`. Fetched and confirmed against the
live `type_checker.cpp` source this session.

## 10. `isDefEq_impl`, `try_struct_eta`
Modeled on the kernel's `try_eta_struct_core`: requires the second term to
be literally constructor-headed, checks the constructor's inductive is a
non-recursive, single-constructor, no-index structure, checks the inferred
types agree, then compares projections pairwise.

## 11. `isDefEq_impl`, `FreeVar, FreeVar` mismatch falls through to `final_fallback`
Directly derived from `quick_is_def_eq`'s explicit `case ... FVar ...:
break;` in `type_checker.cpp` — the kernel deliberately declines to decide
an FVar/FVar mismatch outright, instead falling through to the full generic
sequence below. nyaya previously returned `false` immediately here (a bug,
fixed by copying the kernel's actual control flow). Fetched and confirmed
against the live source this session.

## 12. `isDefEq_impl`, `is_def_eq_unit_like`
Transcribed near-verbatim from the kernel's own `is_def_eq_unit_like`
C++ function (fetched and confirmed against the live source this session):

```cpp
bool type_checker::is_def_eq_unit_like(expr const & t, expr const & s) {
    expr t_type = whnf(infer_type(t));
    expr I = get_app_fn(t_type);
    if (!is_constant(I) || !is_non_rec_structure(env(), const_name(I)))
        return false;
    name ctor_name = head(env().get(const_name(I)).to_inductive_val().get_cnstrs());
    constructor_val ctor_val = env().get(ctor_name).to_constructor_val();
    if (ctor_val.get_nfields() != 0)
        return false;
    return is_def_eq_core(t_type, infer_type(s));
}
```
Any two terms of a non-recursive, single-constructor, zero-field structure
type are declared defeq once their types agree — no independent
verification that this is sound beyond "the kernel does it."

---

## Not kernel-derived (for contrast)

A few items in the same `final_fallback` chain are nyaya-specific and have
no kernel counterpart, included here only so it's clear the boundary was
tracked deliberately, not blurred:
- `try_lam_eta` (lambda eta against an arbitrary term) — homegrown.
- `try_xor_one_step` (one-step `Nat.xor` delta as a last resort) — homegrown,
  works around nyaya's own `is_nat_builtin_name` guard (item 6 above), not
  something the real kernel needs since it doesn't delta-unfold builtins at
  all.

Also, the partial iota rules for `Nat.sub`/`Nat.pow`/etc. with symbolic
arguments are derived from the actual Lean *standard library* source
(`Init/Prelude.lean`'s recursive equations for `Nat.sub`, `Nat.pow`, ...),
not from the kernel's C++. That's a different (and lower-risk) kind of
"peeking" — it's necessary to know the real definition to write a faithful
partial-reduction rule for it at all, not a copy of the type-checking
*algorithm* itself.
