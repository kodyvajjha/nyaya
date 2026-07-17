# nyaya type checker changelog

Progress log for the type checker implementation (`lib/tc.ml` and related files).
Test target: `init.export` (36688 declarations from Lean 4's Init library).

---

## 2026-07-17: short-circuit literal-vs-literal in the Nat.succ offset bridge, clearing `Int16.ofBitVec_intMax`

**File:** `lib/tc.ml`, `isDefEq_impl`'s `nat_offset_bridge` (used by the
lazy-delta loop, `431694c`).

**Problem:** A later discovery sweep (after the `String.append_empty` fix
above) hit a new wall: `Int16.ofBitVec_intMax` failed with
`Depth_limit("[d#2079] depth 2001 exceeds max 2000, input=96311 =?=
30775")` -- the `DefEqTrace` recursion-depth cap (default 2000, separate
from lazy-delta's own 8192 untraced-loop cap), not a hang. `96311 - 30775 =
65536 = 2^16`, i.e. an `Int16`/`BitVec 16` modular-arithmetic byproduct.

Root cause: `nat_offset_bridge`'s `as_nat_succ` treats *any* positive
`NatLit n` as `Nat.succ (n-1)` -- correct when bridging a literal against a
genuinely symbolic `Nat.succ` chain, but when *both* sides are concrete
literals this makes `nat_offset_bridge` peel one unit off both per step via
a real recursive `isDefEq` call (not lazy-delta's local loop), each one a
new `DefEqTrace` frame. For `96311` vs `30775` that's up to 30775 nested,
non-tail calls -- individually cheap, but enough stack frames to blow the
depth cap long before finishing. Confirmed not a hang: raising
`NYAYA_DEFEQ_MAX_DEPTH` to 200000 lets it complete and the declaration
type-checks correctly, taking about the same wall time either way (~30s of
actual work past parsing) -- the fix isn't a speedup, it's avoiding
exhausting the depth budget on a lot of individually-cheap steps. Same root
shape as the `Nat.sub`-on-a-2^32-literal fix (`135fd76`), but that one was
also a real complexity win; this one wasn't.

**Fix:** before the zero/succ dance, check if both sides are `Literal
(NatLit _)` and short-circuit with a direct `Z.equal` comparison (O(1),
zero recursive calls).

**Declaration unblocked:** `Int16.ofBitVec_intMax`.

**Result:** full `dune build @runtest` still exactly 4 known failures, no
regressions.

---

## 2026-07-17: fix missing universe param on synthesized `List.nil`/`List.cons`, clearing `String.append_empty`

**File:** `lib/tc.ml`, `Reduce.string_lit_to_ctor`.

**Problem:** `String.append_empty` failed with `isDefEq` rejecting two terms
that looked, and printed, identical: `String.data (s ++ "")` vs
`String.data s ++ []`. Tracing through (with the shortcut/congruence paths'
logging suppression temporarily disabled to see the silenced sub-comparisons)
found the actual mismatch two levels down: `List.nil` vs `List.nil`, rejected
by the Const-Const defeq case on `List.length us = List.length vs` --
i.e. an arity mismatch on the universe-parameter list, not a name or value
mismatch. One side came from `string_lit_to_ctor` (the kernel-extension
helper that expands a `StrLit` into `String.mk (List.cons ... (List.nil ..))`
for projection reduction, per *Theorem Proving in Lean 4*'s string-literal
projection rule), which built `List.nil`/`List.cons` via `Expr.const` with no
`~ups`, defaulting to an empty uparams list. But `List.{u} : Type u -> Type
u` is universe-polymorphic, so every other occurrence of `List.nil`/
`List.cons` in the kernel carries one universe argument -- a genuine arity
mismatch (0 vs 1 params), not a level-value disagreement. This was invisible
in every trace because `Expr.pp`'s `Const` case hides uparams entirely when
they're empty *or* all-zero (`List.nil.{0}` and `List.nil` print
identically), so the two occurrences looked the same right up until the
length check itself.

**Fix:** pass `~ups:[Level.Zero]` to both `Expr.const` calls (`Char : Type 0`
fixes `u := 0`); `Char.ofNat`/`String.mk`/the bare `Char` type reference are
correctly left at `~ups:[]` since `Char` and `String` are non-polymorphic.

**Declaration unblocked:** `String.append_empty`.

**Result:** full `dune build @runtest` still exactly 4 known failures
(`perf/app-lam`, `perf/grind-ring-5`, `bad/130`, `bad/131`), no regressions.
A follow-on 300s discovery sweep of `init.export` (from the start of the
corpus, since this bug fires on any string-literal projection) completed
2050 declarations cleanly before hitting the time cap -- no new failures.

**Kernel reference:** *Theorem Proving in Lean 4*'s description of the
string-literal kernel extension's projection-reduction special case (convert
a `StringLit` to `String.mk (List Char)` form before proceeding as usual);
`type_checker.cpp`'s `is_def_eq_core` Const-Const case (name and
universe-list equality, arity-checked first).

---

## 2026-07-15: lazy-delta reduction in isDefEq

**Files:** `lib/tc.ml` -- new `whnf_core`/`whnf_core_impl` (delta-free WHNF),
new `find_delta_target`/`unfold_delta_target`/`compare_reducibility_hints`
(module scope, just above `infer`), and a `lazy_delta` loop spliced into
`isDefEq_impl` in place of the old eager `whnf env e1`/`whnf env e2`.

**Problem:** `988bfa7` (soundness-risk #1) correctly removed the unsound
Prop-skip in `isDefEq`, but the cost was `isDefEq` falling back to fully
`whnf`-ing both sides on every comparison -- doing orders of magnitude more
reduction than the kernel, which keeps comparisons shallow via
`lazy_delta_reduction`. This was the confirmed root cause of the
`grind-ring-5` regression (false-reject on depth limit) and a contributor to
`app-lam`'s timeout.

**Fix, ported directly from `type_checker.cpp`** (fetched and read against
primary source, not from memory):
- `whnf_core`: weak-head normal form that never delta-unfolds a `Const` head
  (mirrors the kernel's `whnf_core`). Beta/iota/zeta/projection/native-Nat
  reduction still fire. Separately memoized (`whnf_core_memo`).
  - One kernel subtlety that cost a regression before catching it: a
    recursor's major premise still needs *full* `whnf` (with delta) to see
    through typeclass wrappers like `OfNat.ofNat`/`instOfNatNat` down to
    `Nat.zero` -- that's not delta on the recursor's own head (recursors
    have no `value`, iota is a separate mechanism), it's evaluating a strict
    subterm. The kernel does the same (`cheap_rec` is false even inside
    `is_def_eq_core`). Missing this broke `init-prelude`
    (`Array.appendCore.loop.match_1`) on first pass; fixed by passing `whnf`,
    not `whnf_core`, into `iota_at_head`'s major-premise reduction.
- `isDefEq_impl`'s core loop (`lazy_delta`): whnf_core both sides, then loop
  -- Nat.succ/zero offset bridge (peel one level via a real `isDefEq`
  recursive call, not a literal-magnitude peel) -> native (both-literal-only)
  Nat reduction -> unfold whichever side has lower reducibility height
  (`Def.red_hint`, now finally consumed instead of ignored) -> at equal
  height, try same-declaration arg-by-arg congruence before unfolding both.
  Depth-capped (`NYAYA_LAZY_DELTA_MAX_DEPTH`, default 8192) since this is a
  plain local recursion outside the `MakeTrace` depth-tracked call tree.
- `nat_lit_reduce` gained a `~native_only` mode (both-literal-pairs only, no
  partial-iota) for this loop, matching the kernel's `reduce_nat` exactly --
  the partial-iota extensions are a nyaya-specific addition for whnf outside
  of defeq and stay off here.
- Congruence-failure cache (`congruence_failure_memo`, keyed on tag pairs):
  ports the kernel's `failed_before`/`cache_failure` so a repeated subterm
  pattern (e.g. across a ring-normalization proof) doesn't re-pay for the
  same doomed congruence check every time an enclosing term revisits it.
  Turned out to be load-bearing, not optional -- without it grind-ring-5
  still didn't terminate in reasonable time even with lazy-delta's other
  pieces in place.
- After lazy-delta gets stuck (neither side delta-eligible at the head), one
  more full-`whnf` pass on both sides before falling to the existing
  congruence/eta fallbacks -- mirrors the kernel's second `whnf_core` pass
  "using whnf to reduce projections". Needed because a stuck `Proj` can still
  be hiding a delta-eligible instance underneath (`(instOfNatNat 0).1`).

**Result (measured, `scripts/bench.sh`):**
- `init-prelude`: still accepts; whnf-miss 105114→87953, cpu ~24s→~21s.
- `grind-ring-5`: was killed at the harness's 180s cap with no verdict at
  all; now reaches a verdict (reject, still a false-reject) in ~16.5s.
  whnf-miss 134763→89491 (-34%), delta 32493→24944 (-23%). **Not yet fixed**:
  peak-whnf-depth is still 2001 (still trips the depth cap) even with the
  cap raised 10x (`NYAYA_WHNF_MAX_DEPTH=20000`, still doesn't resolve in
  90s) -- isolating the offending declaration
  (`_private.Test.0.thm._proof_1_1`, same one flagged in the original
  diagnosis) shows the new lazy-delta path resolving quickly for most of the
  corpus, but this one declaration's stuck-projection fallback (the "one
  more full whnf pass" above) still drives a long chain, most likely because
  it's forcing a genuinely large well-founded-recursion-based proof term
  rather than hitting an algorithmic gap in lazy-delta itself. Left open --
  candidate next steps are the union-find/further congruence-caching work
  already on the list, or narrowing the stuck-projection fallback further.
- `app-lam`: still times out (pre-existing, separate beta/instantiation
  issue, not a lazy-delta target -- see `doc/perf-deferrals.md`).
- Full `dune build @runtest`: same 4 known failures as before this change
  (`perf/app-lam`, `perf/grind-ring-5`, `bad/130`, `bad/131`), no new
  regressions across the other 160 cases. Full-suite wall time dropped from
  ~72-85s to ~34-72s across runs (noisy, but consistently at or below the
  pre-change baseline).

**Follow-up same day: `Int.fdiv_eq_ediv` regression + a safety-net fallback.**
Running a broader `init.export` sample (beyond the curated 164-case suite)
surfaced a second, real lazy-delta regression: `Int.fdiv_eq_ediv` (comparing
a `congrArg`-produced proof's type against its `id`-ascribed expected type)
passed before this change and failed after. Root cause: `whnf` continuing
from the lazy-delta loop's `` `Stuck `` output is *not* always equivalent to
independently `whnf`-ing the original two expressions from scratch -- most
likely because `iota_at_head`'s major-premise resolution (specifically its
`is_K` reduction check) can take a different path depending on how much of
the surrounding term lazy-delta already unfolded before handing it off,
versus starting fresh. Rather than chase the exact non-confluence (a deeper
rabbit hole with diminishing returns), added a safety net: if the normal
lazy-delta path raises `Defeq_failure`, retry once against `whnf env e1`/
`whnf env e2` on the *original*, pre-lazy-delta expressions before giving up.
This exactly reproduces the old, proven-correct behavior as an ultimate
fallback -- it can only rescue a case lazy-delta's strategy mishandled, never
mask a real inequality (if the fallback changes nothing, the original
failure is re-raised). Verified: fires 0 times across the full curated
164-case suite (no perf cost there); fixes `Int.fdiv_eq_ediv`; a follow-on
300s discovery-mode sample of `init.export` (alphabetically broad --
`Int`/`UInt`/`List`/`Array`/`BitVec`/`Nat`/`Vector`/`IO`/`Subarray`/`Mod`,
1886 declarations) completed with **zero** new failures before hitting the
time cap.

**Kernel references:** `type_checker.cpp` `whnf_core`, `whnf`, `is_delta`,
`unfold_definition`, `lazy_delta_reduction`, `lazy_delta_reduction_step`,
`is_def_eq_offset`, `reduce_nat`, `is_def_eq_core`, `failed_before`/
`cache_failure`; `declaration.cpp`'s `compare(reducibility_hints, ...)` for
the height-ordering rule (`constant_info::get_hints` confirms Theorem/Opaque
default to the Opaque hint, matching `find_delta_target`'s treatment of
`Decl.Thm`/`Decl.Opaque`).

---

## 2026-07-15: bound the Nat.sub partial-iota peel, fixing the `UInt16.toUInt32_toUInt64` regression

**File:** `lib/tc.ml`, `Reduce.nat_lit_reduce` (`Nat.sub` case) and `whnf_impl` (App
case, head reduction).

**Problem:** The 2026-07-13 diagnosis (see that entry below) identified two
walls in `UInt16.toUInt32_toUInt64`. The first (a projection-head Nat.pow
normalization bug) was fixed in `3a30636`. The second was left open: `Nat.sub`'s
partial-iota rule (`Nat.sub a (succ b) --> Nat.pred (Nat.sub a b)`) recurses on
the subtrahend with **no bound**, unlike the sibling `Nat.add`/`Nat.mul`/`Nat.pow`
partial-iota rules, which all cap the symbolic-second-arg peel at `n <= 64`. On a
symbolic minuend and a `2^32`-scale literal subtrahend (`4294966801`), this peels
one predecessor at a time toward zero -- billions of steps, tripping the 2000-deep
`whnf` depth limit long before making real progress.

**Fix:** Two changes, both scoped to `Nat.sub` only:
1. `nat_lit_reduce`'s `Nat.sub` case now only treats a literal subtrahend as a
   successor (`NatLit k -> pred (NatLit (k-1))`) when `k <= 64`, matching
   add/mul/pow's existing convention. Above the bound, it declines (returns
   `None`) rather than peeling.
2. Once `nat_lit_reduce` declines for a `Nat.sub` application, `whnf_impl`'s App
   case no longer delta-unfolds `Nat.sub`'s own definition (`brecOn`/`Nat.below`)
   to try to make progress another way -- that unfold hits the identical blowup
   via a different path. The term is left stuck instead. This guard is
   deliberately narrow: guarding `mod`/`div`/`modCore` the same way breaks
   `Nat.modCore_lt`, which genuinely needs `modCore` delta-unfolded for some
   symbolic arguments (per the 2026-07-13 note on soundness-risk #1's
   follow-up work, where this exact guard shape was worked out but never
   landed on master before this).

**Declaration unblocked:** `UInt16.toUInt32_toUInt64` (isolated
`NYAYA_ONLY_DECL` check now passes). Full `dune build @runtest` suite unchanged
at 4 known failures (`perf/app-lam`, `perf/grind-ring-5`, `bad/130`, `bad/131`
-- see `doc/soundness-risks.md` #2 and the lazy-delta follow-up tracked
elsewhere), no new regressions.

**Kernel reference:** `type_checker.cpp` `reduce_nat`/`reduce_bin_nat_op` only
special-case Nat literals when both arguments are already literal; a symbolic
minuend with a huge literal subtrahend is not a case the real kernel forces via
unbounded predecessor peeling either. Bounding the peel and declining beyond it
keeps nyaya's behavior narrow (never *more* permissive), matching the
soundness-preserving direction the guard removal in `988bfa7` established.

---

## 2026-07-13: diagnose `UInt16.toUInt32_toUInt64` init regression after `988bfa7`

**File:** `lib/tc.ml`, `nat_lit_reduce`, projection head reduction,
`isDefEq`/lazy-delta follow-up notes.

**Context:** After `988bfa7` removed the old Nat builtin delta guard and the
unsound App-case Prop-skip, isolated checking of
`UInt16.toUInt32_toUInt64` from `test/parser/init.export` started failing
with a WHNF depth-limit while trying to normalize the first real equality in
the proof:

```text
Eq Nat (UInt32.toNat (UInt64.toUInt32 (UInt16.toUInt64 n)))
       (UInt32.toNat (UInt16.toUInt32 n))
=?=
Eq Nat (HMod.hMod Nat Nat Nat (instHMod Nat Nat.instMod)
          (UInt16.toNat n) UInt32.size)
       (UInt16.toNat n)
```

The auto-debug log stops near that comparison because the current
`same_head_args_shortcut` suppresses nested logs. Lower WHNF depth caps and
temporary probes showed three distinct layers:

1. **`Nat.succ` was too syntactic.** The `988bfa7` changelog said
   `Nat.succ` should fold only when its argument is already a literal. That is
   narrower than the kernel/book rule: `Nat.succ n` folds when `n` can be
   reduced to a Nat literal. The implementation now routes `Nat.succ` through
   `as_nat_lit`, preserving the cheap bail-out for obviously symbolic inputs
   but allowing reducible literal arguments such as `OfNat.ofNat Nat 32 ...`.
   This supersedes the earlier changelog wording.

2. **Projection in application-head position was over-normalizing function
   fields.** The wrapper path itself was not the bug. `HPow.hPow` projects a
   field from `instHPow`, then through `Pow.pow`, `instPowNat`, and
   `NatPow.pow` to the `Nat.pow` field of `instNatPowNat`. The old standalone
   projection reducer returned `whnf env field`. When that field was the
   function `Nat.pow`, it unfolded bare `Nat.pow` to its recursive lambda before
   the pending exponent argument was reattached, so the native Nat literal
   reducer never saw the shape `Nat.pow 2 32`.

   The fix is intentionally not a typeclass-wrapper special case: when an
   application head is a projection, `whnf_impl` now reduces the projection to
   the raw field and rebuilds `field args`, allowing the ordinary Nat literal
   path to fire. After this change the following probes all WHNF to
   `4294967296`:

   ```text
   Nat.pow 2 32
   NatPow.pow Nat instNatPowNat 2 32
   Pow.pow Nat Nat (instPowNat Nat instNatPowNat) 2 32
   HPow.hPow Nat Nat Nat (instHPow Nat Nat (instPowNat Nat instNatPowNat)) 2 32
   UInt32.size
   ```

3. **The declaration still fails later on a large symbolic subtraction.** With
   the two fixes above, the isolated command

   ```sh
   env NYAYA_ONLY_DECL=UInt16.toUInt32_toUInt64 \
     dune exec bin/main.exe test/parser/init.export
   ```

   gets past the earlier `Nat.pow`/wrapper blowup, then fails with:

   ```text
   Depth_limit("[w#3211] depth 2001 exceeds max 2000,
     input=Nat.sub (Nat.succ n##43) 4294966801")
   ```

   This is the existing `Nat.sub` partial-iota rule doing exactly what the
   exported Lean definition says for a symbolic minuend and positive
   subtrahend:

   ```lean
   Nat.sub a 0        --> a
   Nat.sub a (succ b) --> Nat.pred (Nat.sub a b)
   ```

   It peels the concrete subtrahend one predecessor at a time, which is
   hopeless for `2^32`-scale literals when the minuend is still symbolic.

**Important non-fix:** Do not "fix" this by adding the tempting paired
decrement shortcut

```text
Nat.sub (Nat.succ n) (Nat.succ m) --> Nat.sub n m
```

as a general reduction rule. Lean 4.16 rejects `rfl` for
`Nat.sub (Nat.succ n) (Nat.succ m) = Nat.sub n m`, and also rejects `rfl` for
`Nat.sub 0 (Nat.succ m) = 0`; these are theorem-level facts, not definitional
reductions for arbitrary symbolic `m`. The earlier changelog entry around
`50af38e` already records why the faithful `Nat.sub a (succ b) -->
Nat.pred (Nat.sub a b)` rule replaced an unsound paired-decrement shortcut.

**Follow-up for lazy-delta work:** The remaining `UInt16.toUInt32_toUInt64`
failure is a reduction-completeness/throughput problem in `isDefEq`, not an
obvious Nat builtin rule. Current `isDefEq` tries a narrow same-head argument
shortcut and then falls back to fully `whnf`-ing both sides. On this declaration
that full WHNF path forces the UInt/BitVec/Fin encoding far enough to expose
the giant symbolic `Nat.sub`. The planned lazy-delta rewrite should keep this
comparison shallow: implement a delta-free `whnf_core`, compare equal-arity
same-head applications by congruence, and otherwise unfold only the side with
greater definitional height using the exported `Def.red_hint` heights. Re-test
this declaration specifically after lazy-delta lands.

**Verification in this state:** `dune build` passes. The wrapper probes above
reduce correctly. `UInt16.toUInt32_toUInt64` still false-rejects at the later
large `Nat.sub` depth-limit, which is the tracked lazy-delta target.

---

## 2026-07-12: O(1) recursion-depth counter in the trace machinery

**File:** `lib/parser/util.ml`, `MakeTrace`

**Problem:** The depth-limit check ran on every `whnf`/`infer`/`isDefEq` entry as
`List.length !stack` — O(depth) per call, so a reduction that recurses to depth
`d` paid O(d²) just accounting for its own depth. On deep reductions this
dominated: measured throughput fell as depth grew (from ~6700 to ~4600
`whnf`/s between depths 9.5k and 21k).

**Fix:** Maintain the depth as an integer counter (`incr` on `enter`, `decr` on
`leave`), used in place of `List.length !stack` for the limit check. The frame
`stack` is retained for debug path display only. Depth semantics are unchanged.
Measured ~1.8× on the deep phase and removes the quadratic degradation; helps
every deep reduction, no behavior change.

---

## 2026-07-12: remove the unsound App-case Prop-skip; clears arena bad/proj-of-prop, bad/nat-rec-rules (soundness-risk #1)

**File:** `tc.ml`, `infer` App case, `whnf_impl`, `nat_lit_reduce`, `isDefEq`

**Problem (the soundness bug):** When the expected parameter type of an
application was a proposition, `infer`'s App case skipped the argument
type-check `isDefEq(arg_type, btype)` entirely and instantiated directly. This
let ill-typed terms through whenever the argument slot was a Prop, so the arena
cases `bad/proj-of-prop` (#1) and `bad/nat-rec-rules` (#2) were **accepted**.
Proof irrelevance is not a license to skip inference — the kernel's `infer_app`
always checks the argument, and irrelevance lives only inside `isDefEq`
(`is_def_eq_proof_irrel`).

**Fix:** Always run `isDefEq env btype arg_type` in the App case, matching
`infer_app`. This is the whole soundness fix; it rejects both cases.

**Fallout — Nat reduction had to become kernel-faithful.** With the skip gone,
`Nat.div`/`Nat.modCore` proofs that were previously never evaluated now have to
reduce. This exposed that the old `is_nat_builtin` delta-guard (which froze all
Nat builtins on symbolic args) was masking real forcing bugs:

- **Removed the `is_nat_builtin` guard** at both `whnf_impl` call sites and
  deleted the helper. The kernel special-cases Nat only on *literals*
  (`reduce_bin_nat_op` bails unless both args are literals) and otherwise gets
  stuck naturally via `reduce_recursor`; it never blanket-freezes symbolic
  delta. The `Nat.xor` case in `nat_lit_reduce` — which had manually restored a
  one-step unfold to work around the guard — is now redundant and reverts to the
  plain `binary` form like `land`/`lor`.
- **Fixed three forcing bugs** the guard had been hiding (nyaya was forcing
  arguments the kernel leaves stuck, driving O(n) unary descent on large
  literals): (1) `Nat.succ` folds into a literal only when its argument is
  *already* a literal — `succ x` with symbolic `x` is already whnf; (2)
  `binary`/`as_nat_lit` bail via `obviously_not_lit` before forcing an argument
  that can never compute to a literal (e.g. `Nat.add (Nat.mul a k) a` with
  symbolic `a`); (3) a `NatLit`-vs-`succ`-chain defeq bridge (`try_natlit_succ`),
  needed because whnf no longer eagerly folds a `succ` chain into a literal —
  it peels leading `succ`s in a flat loop and matches `n - count` against the
  base.

The recursor-major-premise path is unchanged: a `NatLit n` major premise is
still exposed one step to `Nat.zero`/`Nat.succ (NatLit (n-1))` (kernel
`nat_lit_to_constructor`), never fully unrolled.

**Kernel reference:** `src/kernel/type_checker.cpp` `infer_app` (always checks
the argument) and `reduce_bin_nat_op` (literal-only); `src/kernel/inductive.cpp`
`nat_lit_to_constructor` (one-step literal exposure).

**Cases unblocked:** arena `bad/proj-of-prop`, `bad/nat-rec-rules`.
`init-prelude`, `098_natLitEq` and the rest of the good corpus stay green.

**Known regression — `good/perf/grind-ring-5`.** This valid file was green only
because the Prop-skip skipped the very obligations it should have checked. With
the skip gone it now trips the `whnf` depth limit on a deep reduction and
**false-rejects** (exit 1); the reduction reaches depth 20000+ and keeps
climbing (measured ~524k `whnf` calls in 63s, nowhere near done). This is a
reduction *completeness/throughput* gap, not a soundness regression: the change
trades a former false-*accept* (the unsound Prop-skip) for a surfaced
false-*reject*, so soundness strictly improves.

Root-caused for follow-up: `isDefEq` (after a narrow same-head shortcut) falls
back to fully `whnf`-ing both sides, where the kernel runs
`lazy_delta_reduction` — compare heads, congruence on equal-arity same-head
applications, and otherwise unfold only the side with greater definitional
height by one step. That keeps the kernel shallow on exactly these terms. The
definitional-height hints are already exported and parsed into `Def.red_hint`
but currently ignored (`tc.ml` `Def` case, `TODO`). Implementing lazy-delta
(plus a delta-free `whnf_core`) is the tracked fix; it is a focused rewrite of
the `isDefEq`/`whnf` core, held as its own follow-up rather than blocking this
soundness fix. Distinct from `app-lam`'s pure timeout.

---

## 2026-07-12: strict positivity for non-nested inductives; clears arena bad/051, 054, 111

**File:** `tc.ml`, `check_ctor`

**Problem:** A constructor field in which the inductive occurs to the left of an
arrow (a non-positive occurrence) makes the type logically inconsistent. This
was the remaining unchecked constructor obligation, so the arena cases
`051_indNeg` (`(I → I) → I`), `054_indNegReducible` (a negative occurrence
hidden behind a reducible `constType aType I → I`), and `111_reflOccLeft`
(`Nat → (I → Nat)`) were **accepted**.

**Fix:** Add `check_positivity`, run on each constructor field. It reduces the
field to head-normal form (so a negative occurrence that could otherwise be
reduced away, as in `054`, still counts) and then: a function type's domain may
not mention the inductive (recurse into its result), a plain recursive
application is fine, and anything else mentioning the inductive is rejected.

**Gated on `num_nested = 0`.** The real kernel un-nests nested inductives into
fresh parameters before checking positivity; nyaya does not un-nest, so a naive
check would false-reject a valid nested field such as `List I`. The
`num_nested` count (from the export) is exactly the kernel's signal for whether
un-nesting is needed, so positivity runs only when it is zero. Nested
inductives therefore still skip the check — a remaining over-permissive gap, now
the sole content of `doc/soundness-risks.md` #3.

Verified no false-reject on valid recursive inductives in the good corpus
(`035_twoBool`, `052_reduceCtorParam` with recursive `constType (I α) (I α)`
fields, the `*Rec` cases, etc. all still accept).

**Kernel reference:** `src/kernel/inductive.cpp` `check_positivity` (whnf; `pi`
with an inductive-free domain recurses; a valid recursive application is
accepted; otherwise reject) and its `!m_is_unsafe` / nested-inductive handling.

**Cases unblocked:** arena `bad/tutorial/051_indNeg`, `054_indNegReducible`,
`111_reflOccLeft` (8 → 5 red).

---

## 2026-07-12: restrict projections out of a proposition; clears arena bad/085, 087, 088, 089

**File:** `tc.ml`, `infer` `Proj` case

**Problem:** A structure that lives in `Prop` is a subsingleton — proof
irrelevance equates all its inhabitants — so projecting a *data* (non-`Prop`)
field out of it is unsound: it would let distinct data be proved equal. nyaya
computed the projection's type but never enforced this, so the arena `projProp`
cluster (all expected **reject**) was **accepted**: `085_projProp2` /
`087_projProp4` project data fields directly, and `088_projProp5` /
`089_projProp6` project fields sitting after a depended-upon data field.

**Fix:** After computing whether the structure's type is a `Prop`
(`is_prop_type`), add the two guards the kernel applies:
- when peeling each earlier field, if a later field depends on it
  (`num_loose_bvars body > 0`) and it is data, reject — a Prop structure may not
  carry a depended-upon data field before the projected one (`088`, `089`);
- the projected field's own type must be a `Prop` (`085`, `087`).

Non-`Prop` structures are unaffected (both guards are gated on `is_prop_type`),
and proof projections that only follow non-dependent data stay valid
(`good/tutorial/084_projProp1`, `086_projProp3` still accept).

**Kernel reference:** `src/kernel/type_checker.cpp` `infer_proj` — the
`is_prop_type && !is_prop(binding_domain(r))` check inside the pre-field loop
(guarded by `has_loose_bvars(binding_body(r))`) and the final
`is_prop_type && !is_prop(r)` check on the projected field.

**Cases unblocked:** arena `bad/tutorial/085_projProp2`, `087_projProp4`,
`088_projProp5`, `089_projProp6` (12 → 8 red).

---

## 2026-07-12: validate constructor headers + field universes; clears arena bad/047–050, 053, 058

**File:** `tc.ml`, `check_ctor` (was five stubs hardcoded `true`)

**Problem:** `check_ctor` validated nothing about a constructor — every
obligation returned `true`, so the arena cluster `bad/tutorial/047–058` (all
expected **reject**) was **accepted**. The defects: a constructor whose
parameter binder does not match the inductive's (`047`), whose result applies
the inductive to the wrong parameters (`048`) or wrong universe levels (`049`)
or leaves the inductive occurring in a result index (`050`), whose type reduces
to — but is not manifestly — the inductive (`053`), or whose field lives in a
universe higher than the inductive's (`058`).

**Fix:** Port the header-consistency and universe half of Lean's
`check_constructors`. Peel the constructor's telescope *without* reducing it
(matching the kernel's `while (is_pi(t))`): def-eq each parameter binder against
the inductive's shared parameters, universe-check each field
(`sort ≤ inductive level`, or the inductive is a `Prop`), and require the
manifest result to be `is_valid_ind_app` — head is the inductive applied (with
its own universe params, compared structurally by name) to exactly the shared
parameter fvars, with no index mentioning the inductive.

**Deliberately not done — strict positivity (`051`, `054`).** The kernel's
`check_positivity` runs only after nested inductives are un-nested; nyaya does
not un-nest, so a naive port would false-reject valid nested inductives in the
good corpus. Those two purely-positivity cases stay deferred (see
`doc/soundness-risks.md` #3, now narrowed to just positivity).

**Kernel reference:** `src/kernel/inductive.cpp` — `check_constructors` (the
`is_def_eq(binding_domain, get_param_type)` parameter check, the
`is_geq(m_result_level, sort_level(s)) || is_zero(m_result_level)` universe
check) and `is_valid_ind_app` (head/param/index-occurrence check).

**Cases unblocked:** arena `bad/tutorial/047_inductWrongCtorParams`,
`048_inductWrongCtorResParams`, `049_inductWrongCtorResLevel`,
`050_inductInIndex`, `053_reduceCtorType`, `058_typeWithTooHighTypeField`
(18 → 12 red). `051_indNeg` and `054_indNegReducible` remain (positivity).

---

## 2026-07-12: projection type must be a single-constructor structure; clears arena bad/083_projNotStruct

**File:** `tc.ml`, `infer` `Proj` case

**Problem:** `bad/tutorial/083_projNotStruct` projects field 0 of an `x : N`
where `N` is a two-constructor inductive (`zero`/`succ`). A projection is only
valid on a *structure* — an inductive with exactly one constructor. nyaya
`assert`ed `CCList.length ctor_names = 1`, so it **crashed** (assertion failure,
checker-error exit 3) instead of rejecting.

**Fix:** Replace the assertion with a real check: if the projected type's
inductive does not have exactly one constructor, raise `TypeError` (→ `Reject`).

**Kernel reference:** Lean's `infer_proj` (`src/kernel/type_checker.cpp`) throws
`invalid_proj_exception` when `length(I_val.get_cnstrs()) != 1` (also when the
head is not a constant/inductive, or the argument count ≠ params+indices).

**Case unblocked:** arena `bad/tutorial/083_projNotStruct` (19 → 18 red).

## 2026-07-12: reject duplicate declaration names; clears arena bad/126–129, 133

**File:** `env.ml` (new `duplicates` field + detection), `tc.ml`
`check_env_verdict`

**Problem:** The arena `dup_*` cases declare the same name twice (e.g.
`126_dup_defs` ships two identical `def dup_defs`; `128_dup_ctor_def`,
`129_dup_rec_def`, `133_DupConCon` duplicate a constructor/recursor name). nyaya
accepted them: env construction stores declarations in a table built with
`Hashtbl.add` and resolution dedupes multiple bindings of a name into one
`resolved_table` entry, so the collision was silently dropped and never became a
verdict.

**Fix:** During env construction, detect any name id with more than one binding
in `decl_table` and record the offending `Name.t`s in a new `Env.t.duplicates`
field. `check_env_verdict` returns `Reject` when it is non-empty.

**Kernel reference:** Lean's `environment::add` (`src/kernel/environment.cpp`)
throws `already_declared_exception` when a declaration's name is already present
in the environment — a duplicate name makes the file invalid.

**Cases unblocked:** arena `bad/tutorial/126_dup_defs`, `127_dup_ind_def`,
`128_dup_ctor_def`, `129_dup_rec_def`, `133_DupConCon` (24 → 19 red).
`130_misnamed_rec_user` and `131_dup_rec_def2` encode a different defect and
remain.

## 2026-07-12: inductive type must be an arity ending in a sort; clears arena bad/044_inductBadNonSort2

**File:** `tc.ml`, `check` `Inductive` case (was hardcoded `true`)

**Problem:** `bad/tutorial/044_inductBadNonSort2` declares an inductive whose
`type` is `aType` — a `const` naming an axiom of type `Sort 1`. That type is
perfectly well-typed (so `well_posed`, which only checks `infer info.ty` is a
sort, passed), but it is not itself a *sort*, and an inductive's type must be an
*arity*: a Pi-telescope of parameters/indices ending in `Sort u`. nyaya's
`Inductive` check was a hardcoded `true`, so it accepted this.

**Fix:** In `check`'s `Inductive` branch, whnf the type and peel every `Forall`
(instantiating the body with a fresh free var), then require the conclusion to
be a `Sort`; otherwise raise `TypeError` (→ `Reject`).

**Kernel reference:** Lean's `check_inductive_types` (`src/kernel/inductive.cpp`)
does exactly this — `type = whnf(type); while (is_pi(type)) { … type = instantiate(binding_body(type), …); type = whnf(type); } … type = ensure_sort(type);` — where `ensure_sort` throws if the conclusion is not a sort.

**Case unblocked:** arena `bad/tutorial/044_inductBadNonSort2` (26 → 25 red).

## 2026-07-12: inductive arity must have exactly numParams+numIndices binders; clears arena bad/046_inductTooFewParams

**File:** `tc.ml`, `check` `Inductive` case

**Problem:** `bad/tutorial/046_inductTooFewParams` declares `numParams = 2` but
its type is a single `Π (x : Prop), Prop` — one binder. The previous arity
check (added for 044) peeled *all* Pis and only checked the conclusion was a
sort, so it accepted a type with fewer binders than the declared parameter
count.

**Fix:** Generalize the arity check to a counted peel: require exactly
`num_params + num_idx` leading `Forall`s (each must be present after whnf),
then require the conclusion to be a `Sort`. This subsumes the 044 case
(`num_params + num_idx = 0`, so the type itself must be a sort).

**Kernel reference:** Lean's `check_inductive_types` (`src/kernel/inductive.cpp`)
peels the parameters and indices one Pi at a time and throws "number of
parameters mismatch" if it runs out of Pis before consuming all declared
parameters/indices, then `ensure_sort(type)`.

**Case unblocked:** arena `bad/tutorial/046_inductTooFewParams` (25 → 24 red).

## 2026-07-12: theorem type must be a proposition; clears arena bad/011_nonPropThm

**File:** `tc.ml`, `check` `Thm` case

**Problem:** The arena case `bad/tutorial/011_nonPropThm` is a `thmDecl` whose
`type` is `Sort 0` (i.e. `Prop`) — but `Prop`'s own type is `Sort 1`, so `Prop`
is not a proposition, and a theorem's type must be one. nyaya accepted it: the
`Thm` branch only inferred the proof's type and checked it defeq to the
declared type (which succeeds here — the proof `∀ x : Prop, x` does have type
`Prop`), but never checked that the *type itself* is a proposition.

**Fix:** In `check`'s `Thm` branch, before checking the proof, require the
theorem's type to be a proposition: `whnf (infer info.ty)` must be `Sort 0`,
else raise `TypeError "theorem type is not a proposition"` (→ `Reject`).
Definitions (`Def`) deliberately keep no such restriction.

**Kernel reference:** Lean's `environment.cpp` `add_theorem` performs exactly
this check, and only for theorems:
`if (!checker.is_prop(type)) throw theorem_type_is_not_prop(*this, v.get_name(), type);`
where `is_prop(e) = whnf(infer_type(e)) == mk_Prop()`
(`type_checker.cpp`). The safe-definition path omits it.

**Case unblocked:** arena `bad/tutorial/011_nonPropThm` (27 → 26 red).

## 2026-07-11: `infer` rejects (not crashes) on an unknown constant; clears arena bad/large-elim-param

**File:** `tc.ml`, `infer_impl` `Const` case

**Problem:** The arena soundness case `bad/large-elim-param` builds an
inductive `MyBool.{u} : Sort u | tt | ff` and then references
`MyBool.rec.{1,0}` in a proof of `False` — but the export ships MyBool with an
empty `recs` list, so `MyBool.rec` is never added to the environment (nyaya
reads recursors from the export; it does not synthesize them). Inferring the
type of `Const MyBool.rec` did a bare `Hashtbl.find env.tbl name`, which raised
OCaml's `Not_found`. That exception is not one of the arena verdict handler's
recognized rejection exceptions, so it escaped to the entry point and became a
checker-error exit code (3) instead of a reject (1). The case therefore failed
even though the correct verdict — reject — was reachable.

**Fix:** In the `infer` `Const` case, look the name up with `Hashtbl.find_opt`
and, on `None`, raise `TypeError "infer Const: unknown constant"` (via
`Logger.err`). `check_env_verdict` maps `TypeError` to `Reject`, so a reference
to an undeclared constant now rejects the file cleanly.

**Kernel reference:** Lean's `type_checker::infer_constant`
(`src/kernel/type_checker.cpp`) infers a constant's type via
`constant_info info = env().get(const_name(e));`, and `environment::get`
(`src/kernel/environment.cpp`) throws when the name is absent:
`object * o = lean_environment_find(...); if (is_scalar(o)) throw unknown_constant_exception(*this, n);`.
A reference to a nonexistent constant is thus a kernel-level type error that
fails the declaration — exactly a `Reject`, never a checker bug.

**Case unblocked:** arena `bad/large-elim-param` (28 → 27 red).

## 2026-07-11: level `leq` normalizes both sides at entry; clears arena good/012–014 (levelComp1/2/3)

**File:** `level.ml`, `( <= )`

**Problem:** The arena cases `good/tutorial/012_levelComp1` (`Sort (imax 1 0)`
at type `Sort 1`), `013_levelComp2` (`imax 0 1`), and `014_levelComp3`
(`imax 2 1`) crashed the checker (`exit 3`) with
`leq : not defined for that case where x,y = (imax(1,0),0)!`. `leq`'s case
analysis has no arm for an `IMax` whose second argument is a concrete
non-parameter level (`Zero` or `Succ _`), so it fell through to the
`Logger.err`/`Failure "leq"` catch-all. Such forms only reach `leq` because
the entry point `( <= )` passed its arguments straight to `leq` without
normalizing — the sort-defeq check compares `succ (imax 1 0)` against
`succ 0`, descending to `leq (imax 1 0) 0`.

**Fix:** Normalize both operands with `simplify` before entering `leq`:
`let ( <= ) l1 l2 = leq (simplify l1) (simplify l2) 0`. `simplify` already
collapses `imax` with a concrete second argument (`imax _ 0 → 0`,
`imax 1 r → r`, `imax a (succ b) → max a (succ b)`), so no `IMax(_, Zero|Succ)`
survives to `leq`; the remaining post-normalization `IMax` forms all have a
parameter (or nested-imax/max) second argument, which the existing arms
handle.

**Kernel reference:** `src/kernel/level.cpp` normalizes at exactly this
boundary rather than inside the core recursion:
`bool is_geq(level const & l1, level const & l2) { return is_geq_core(normalize(l1), normalize(l2)); }`
and `bool is_equivalent(level const & lhs, level const & rhs) { return lhs == rhs || normalize(lhs) == normalize(rhs); }`.
`is_geq_core` (the analog of nyaya's `leq`) therefore assumes normalized
inputs. `simplify` is nyaya's `normalize`.

**Case unblocked:** arena `good/tutorial/012_levelComp1`,
`013_levelComp2`, `014_levelComp3` (31 → 28 red).

## 2026-07-10: is_def_eq_unit_like, and FreeVar/FreeVar mismatch falls through to it; clears PUnit.ext/Unit.ext, `init.export` now 36688/36688

**File:** `tc.ml`, `isDefEq_impl`

**Problem:** `PUnit.ext`/`Unit.ext` (`∀ a b : PUnit, a = b`, proved by a bare
`rfl` on the first argument, i.e. inferred type `Eq PUnit x x` vs declared
`Eq PUnit x y`) failed. Tracing it down: the mismatch bottoms out at
comparing the two bound variables `x` and `y` directly, which hits nyaya's
`Expr.FreeVar, Expr.FreeVar` match arm. On an fvarId mismatch that arm
returned `false` outright — a dead end, never trying struct-eta or any
other fallback.

**Kernel reference:** `kernel/type_checker.cpp`'s `quick_is_def_eq`
explicitly declines to decide `FVar` (and `BVar`/`App`/`Const`/`Let`/`Proj`)
by kind — `case ... FVar ...: break;` falls through to `l_undef`, not
`l_false`. `is_def_eq_core` only returns `true` early for *matching* fvar
ids (`is_fvar(t_n) && is_fvar(s_n) && fvar_name(t_n) == fvar_name(s_n)`);
a mismatch falls through the entire remaining sequence
(`is_def_eq_app` → `try_eta_expansion` → `try_eta_struct` →
`try_string_lit_expansion` → **`is_def_eq_unit_like`**) before finally
returning `false`. nyaya had no equivalent of that last step at all:

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
Any two terms of a non-recursive, single-constructor, **zero-field**
structure type (`PUnit`, `Unit`, `True`, ...) are defeq unconditionally —
the type has exactly one canonical inhabitant, so once the types agree
there is nothing left to compare. This is a strictly narrower/different
condition than `try_eta_struct`, which needs one side to already be
literally constructor-headed (fine for e.g. `Prod`, useless here since
neither `x` nor `y` is syntactically `PUnit.unit`).

**Fix:**
1. Added `is_def_eq_unit_like`, mirroring the C++ above: whnf the first
   term's type, require its head to be a non-recursive single-constructor
   inductive whose (sole) constructor has `num_fields = 0`, then just
   compare the two terms' types via `isDefEq`.
2. Hoisted `try_lam_eta`/`try_xor_one_step` (previously defined inline
   inside the catch-all arm) alongside `try_struct_eta`, and combined all
   four into one `final_fallback` closure ending in the existing
   `Defeq_failure` raise.
3. The catch-all arm (`| _ -> ...`) now just calls `final_fallback`.
4. The `FreeVar, FreeVar` arm's mismatch case now also calls
   `final_fallback` instead of returning `false` directly — the one
   behavioral change, matching the kernel's actual control flow for this
   node kind exactly.

The `App, App` arm is untouched: it deliberately returns `false` rather
than raising (it's used as a nested speculative sub-check elsewhere), so it
keeps its own local `try_struct_eta`-only fallback rather than calling
`final_fallback`.

**Verification:** `dune runtest` (17 small export files) unchanged, all
pass. `NYAYA_SWEEP_ALL=1 dune exec bin/main.exe --root=.` (full
36688-declaration corpus, ~3m10s): **36688/36688 pass, 0 failing** — up
from 36686/36688 (`PUnit.ext`/`Unit.ext` failing) before this fix, and up
from 36682/36688 before the preceding `Quot` fix. `init.export` is now
fully checked, start to finish, with zero exceptions and zero silent
`check`-returns-`false` gaps.

**Declarations unblocked:** `PUnit.ext`, `Unit.ext` (the last two).

---

## 2026-07-10: Quot.ind/Quot.lift computation rules; `check` handles `Decl.Quot`; clears the quotient wall

**File:** `tc.ml`, `iota_at_head` (new `Decl.Quot` case) and `check` (new `Quot` case)

**Problem:** two separate gaps, both scoped to `Quot`:
1. `whnf`'s `iota_at_head` only special-cased `Decl.Rec` (ordinary inductive
   recursors). `Quot.ind`/`Quot.lift` have no `Decl.Rec` entry and no
   `value` to delta-unfold (`Decl.Quot` carries only a type) — applications
   like `Quot.lift f h (Quot.mk r a)` were simply stuck, so anything built
   on `Quotient`/`Setoid` (43 declarations, previously excluded from the
   sweep via `NYAYA_SKIP_PREFIX="Quot,Quotient"`) couldn't be checked.
2. `check` had no case for `Decl.Quot` at all — it fell into the old
   catch-all `_ ->` arm and raised unconditionally, so even `Quot`,
   `Quot.mk`, `Quot.lift`, `Quot.ind` themselves (which have no value to
   check, same as `Axiom`) failed outright.

**Fix:**
1. Added a `Decl.Quot` arm to `iota_at_head` implementing the two kernel
   computation rules verbatim from "Type Checking in Lean 4" /
   `kernel/type_checker.cpp`'s `reduce_quot_rec`: for `Quot.ind` (major
   premise at spine index 4) and `Quot.lift` (major premise at spine index
   5), whnf the major; if it's headed by `Quot.mk` with exactly 3 args
   (`α`, `r`, `a`), the reduct is `f a` (`f` = spine arg 3) with any
   trailing spine args re-applied. No universe/level substitution is
   needed here (unlike ordinary `Rec_rule`s) since `f`/`a` are taken
   directly from the actual application, not a stored template.
2. Added a `Quot` arm to `check` mirroring `Axiom`: no value, so nothing
   to check beyond well-posedness (already verified separately).

**Verification:** `NYAYA_SWEEP_ALL=1 dune exec bin/main.exe --root=.`
(no `NYAYA_SKIP_PREFIX` — full 36688-declaration corpus, ~3m15s) went from
43 declarations excluded outright to all `Quot`/`Quotient` declarations
checked and passing. Remaining failures: `PUnit.ext`/`Unit.ext` only —
pre-existing and unrelated to `Quot` (see below), already present before
this fix but silently swallowed by the plain discovery walk's
success-counter quirk (the "gap of 10" noted in the previous entry), only
surfaced now because `NYAYA_SWEEP_ALL` treats a `check` returning `false`
without raising as a recorded failure rather than a silent no-op.

**Known remaining gap (not fixed here, out of scope for quotients):**
`PUnit.ext`/`Unit.ext` fail because `x =?= y` for two free variables of a
zero-field-constructor type (`PUnit`/`Unit`) is never tried via struct-eta
at the point they're compared — congruence decomposes
`Eq PUnit x x =?= Eq PUnit x y` down to comparing `x` against `y`, but
that sub-comparison isn't logged/attempted before the whole-term
struct-eta fallback (which doesn't apply, since `Eq _ _ _` itself isn't a
struct type) gives up. Needs struct-eta to also fire when comparing two
bare free variables of a subsingleton type, not just App-headed terms.

**Declarations unblocked:** all 43 previously-skipped `Quot`/`Quotient`
declarations (`Quot`, `Quot.mk`, `Quot.lift`, `Quot.ind`, `Quot.sound`,
`Quotient.mk`, `Quotient.lift`, `Quotient.ind`, `Quotient.sound`, and
downstream users) now pass. Confirmed via a full unrestricted
`NYAYA_SWEEP_ALL=1` run: 36686/36688 pass, 2 failing (`PUnit.ext`,
`Unit.ext` — the pre-existing unrelated gap above, not `Quot`-prefixed and
not part of the 43). `init.export` is now fully checked start to finish
modulo those 2 declarations.

---

## 2026-07-10: isDefEq falls through to struct-eta after App/App congruence fails; unblocks StateT.run_modifyGet (and, transitively, the rest of init.export)

### isDefEq_impl: App/App branch now tries struct-eta on the whole terms before giving up
**File:** `tc.ml`, `isDefEq_impl` (`Expr.App, Expr.App` case; `try_struct_eta` hoisted)
**Problem:** `StateT.run_modifyGet` failed with `isDefEq: structural
mismatch`, ultimately reducing to comparing `f s` (a stuck application,
`f` a free variable of function type into `Prod`) against
`Prod.mk α σ (Prod.fst α σ (f s)) (Prod.snd α σ (f s))` -- the structure-eta
expansion of `f s` itself, which should trivially succeed. `nyaya` already
had a `try_struct_eta` fallback (added in an earlier fix), but it only ever
ran from isDefEq_impl's final catch-all (`_ ->`) arm -- which is *skipped
entirely* whenever both compared terms happen to already match the
`Expr.App (f, a), Expr.App (g, b)` pattern (an exclusive OCaml pattern
match commits to the first matching arm). Since `f s` and
`Prod.mk α σ p1 p2` are both `Expr.App` nodes, they matched the App/App
arm, which decomposes the spine ONE ARGUMENT AT A TIME (`fn_eq` on `f` vs
`Prod.mk α σ p1`, `arg_eq` on `s` vs `p2`) rather than treating the whole
terms as a unit -- and a partially-applied constructor (missing one field)
can never itself satisfy `try_struct_eta`'s "fully applied" precondition.
Worse, the recursive `fn_eq` sub-comparison (`f` vs the partial ctor
application) falls into isDefEq_impl's OWN catch-all, where every fallback
also fails, so it *raises* `Defeq_failure` via `Logger.err` -- and because
the App/App branch's own `isDefEq ... && isDefEq ...` congruence check
doesn't catch that exception, it escapes uncaught, aborting the whole
top-level comparison before struct-eta on the *original*, fully-applied
`f s` / `Prod.mk α σ p1 p2` terms ever gets a chance to run.

**Kernel reference (found before writing the patch):** Lean 4's kernel
performs this in the opposite order intentionally. `src/kernel/
type_checker.cpp`'s `is_def_eq_core` (fetched via
`raw.githubusercontent.com/leanprover/lean4/master/src/kernel/type_checker.cpp`):
```cpp
if (is_def_eq_app(t_n, s_n))
    return true;
if (try_eta_expansion(t_n, s_n))
    return true;
if (try_eta_struct(t_n, s_n))
    return true;
```
`is_def_eq_app` (same file) flattens the FULL application spine on both
sides in one shot (`get_app_args`) and requires equal arity plus every
corresponding argument pairwise defeq -- it is a plain bool-returning
function, so a failed sub-comparison anywhere just contributes to an
overall `false`, it cannot abort the caller by exception. Only once
`is_def_eq_app` returns `false` as a whole does the kernel try
`try_eta_struct` (`type_checker.cpp`'s `try_eta_struct_core`, wrapped
symmetrically by `try_eta_struct` in `type_checker.h`) -- and crucially,
it tries this on `t_n`/`s_n`, the SAME two full terms `is_def_eq_app` was
just given, never on a partially-decomposed sub-application. This ordering
is exactly what makes `f s =?= Prod.mk α σ (fst (f s)) (snd (f s))`
succeed in the real kernel: `is_def_eq_app` fails outright (different
heads, `f` vs `Prod.mk`), then `try_eta_struct_core(f s, Prod.mk ...)`
succeeds directly on the whole terms (checks arity `nparams+nfields`,
non-recursive-structure, `infer_type` agreement, then compares
`Proj(Prod,0,f s)` against `fst (f s)` and `Proj(Prod,1,f s)` against
`snd (f s)`, both trivially true).

**Fix:** two changes to make nyaya's App/App branch behave like the
kernel's `is_def_eq_app` + `try_eta_struct` sequence instead of a single
committed pattern-match arm with no fallback:
1. `try_struct_eta` is hoisted out of the catch-all `_ ->` arm to just
   above the `match`, so it's available to both the catch-all (unchanged
   behavior there) and the App/App arm.
2. The App/App arm's own congruence check (`fn_eq && arg_eq`) is wrapped
   in the same speculative-attempt pattern already established for
   `same_head_args_shortcut` above it (suppress logs, catch
   `Defeq_failure` as `false`) so a deep recursive failure inside it
   cannot escape as an uncaught exception -- mirroring that
   `is_def_eq_app` in the real kernel is a plain bool, never a throw. If
   that congruence fails (cleanly, as `false`), the arm now tries
   `try_struct_eta e1' e2' || try_struct_eta e2' e1'` on the WHOLE
   already-whnf'd terms (not the decomposed fn/arg) before finally
   returning `false` -- exactly matching the kernel's fallback order.

This is not a new heuristic or a broadened rule: it is the existing,
already-cited `try_struct_eta` (itself previously verified against
`is_non_rec_structure`/`get_app_num_args` preconditions matching the
kernel) reachable from one more place it should always have been reachable
from, since the kernel tries it unconditionally as a fallback regardless
of whether the two compared terms happen to both currently be `App` nodes.

**Verification:** fast-tier equivalent -- `NYAYA_SKIP_PREFIX="Quot,Quotient"
dune exec bin/main.exe --root=.` (single process, one parse, walks the
whole hashtable) no longer stops at `StateT.run_modifyGet` or anywhere
else: it ran to completion and printed
`Successfully checked 36635 declarations in environment` with zero raised
`TypeError`/`Defeq_failure`/`Depth_limit`/`Not_well_posed` exceptions
across the entire corpus (36688 total, 43 skipped as `Quot`/`Quotient`,
zero failures). Noting honestly, not silently: `36688 - 43 = 36645`
expected-processed vs `36635` counted successes leaves a gap of 10: this
traces to a **pre-existing, unrelated** quirk in `check` (`Def`/`Thm`/
`Opaque` cases print "Successfully type-checked" and return `ans`
*unconditionally*, so if `ans` itself is `false` -- i.e. the top-level
`isDefEq inferred_type declared_type` call returns `false` cleanly rather
than raising -- the per-declaration handler's `if check env d then ...
else ()` silently does neither the success-count increment nor the
exception/debug-file path), predating every fix in this changelog, not
something introduced or masked by this change. Not chased further here
(out of scope for this fix; flagged for awareness, not silently dropped).
No regression: this is the same plain discovery-walk mechanism used to
verify every earlier fix in this document, now covering the entire corpus
in one pass rather than stopping partway.
**Kernel reference:** `src/kernel/type_checker.cpp`'s `is_def_eq_core`,
`is_def_eq_app`, `try_eta_struct_core` (quoted above) and `src/kernel/
type_checker.h`'s `try_eta_struct`/`try_eta_expansion` wrappers (fetched via
`raw.githubusercontent.com/leanprover/lean4/master/src/kernel/type_checker.h`).
**Declaration unblocked:** `StateT.run_modifyGet` (and no further failures
found anywhere else in `init.export` outside `Quot`/`Quotient`, as of this
commit -- see verification note above on the small unrelated counting gap).

---

## 2026-07-10: K-like reduction for subsingleton recursors (Eq/HEq/...); unblocks cast_eq

### iota_at_head: implement K-reduction for is_K recursors, matching to_cnstr_when_K
**File:** `tc.ml`, `iota_at_head` (`Decl.Rec` case)
**Problem:** `cast_eq` (`cast rfl a = a`, i.e. `cast.{u} α α h a = a`) failed
with `isDefEq: structural mismatch`, `lhs = cast α α h a` vs `rhs = a`.
`cast` is defined via `Eq.mpr`/`Eq.rec` (ultimately `Eq.ndrec`/`Eq.rec`)
applied to a proof `h : α = α` that is a free variable, not literally
`Eq.refl`/constructor-headed -- ordinary iota can't fire because the major
premise of the `Eq.rec` isn't whnf-reducible to a constructor application
at all when the equality proof is symbolic. `nyaya`'s parser already reads
and stores an `is_K : bool` flag per recursor (`Decl.Rec.is_K` in
`decl.ml`) but nothing consumed it -- `iota_at_head` treated every
recursor identically (require the major premise to whnf to a literal
constructor application), so any `Eq.rec`/`HEq.rec`-style elimination with
a non-constructor-headed (e.g. free-variable) major premise was
permanently stuck, regardless of what the *type* of that premise could
prove about it.

**Kernel reference (found before writing the patch):** Lean 4's kernel
implements exactly this as "K-like reduction," in
`src/kernel/inductive.h` (fetched via
`raw.githubusercontent.com/leanprover/lean4/master/src/kernel/inductive.h`):
```cpp
template<typename WHNF, typename INFER, typename IS_DEF_EQ>
inline expr to_cnstr_when_K(environment const & env, recursor_val const & rval, expr const & e,
                            WHNF const & whnf, INFER const & infer_type, IS_DEF_EQ const & is_def_eq) {
    lean_assert(rval.is_k());
    expr app_type    = whnf(infer_type(e));
    expr const & app_type_I = get_app_fn(app_type);
    if (!is_constant(app_type_I) || const_name(app_type_I) != rval.get_major_induct()) return e;
    ...
    optional<expr> new_cnstr_app = mk_nullary_cnstr(env, app_type, rval.get_nparams());
    if (!new_cnstr_app) return e;
    expr new_type    = infer_type(*new_cnstr_app);
    if (!is_def_eq(app_type, new_type)) return e;
    return *new_cnstr_app;
}
```
called from `inductive_reduce_rec` (same file) as:
```cpp
if (rec_val.is_k()) {
    major = to_cnstr_when_K(env, rec_val, major, whnf, infer_type, is_def_eq);
}
major = whnf(major);
```
i.e.: for a recursor flagged `is_k`, before whnf'ing the major premise the
kernel infers its *type*, and if that type's head is (an application of)
the recursor's own inductive with the SAME parameters, it synthesizes the
inductive's (unique) nullary constructor applied to those same parameters
(`mk_nullary_cnstr`, `src/kernel/inductive.cpp`: take the type's head
constant, its first/only constructor, apply the type's own leading
`num_params` arguments) and substitutes that in place of the stuck major
premise -- but ONLY if a final `is_def_eq(app_type, new_type)` check
passes, confirming the synthesized constructor really does inhabit the
same type as the original stuck term. `rval.is_k()`/`is_K` is itself only
ever set true at declaration time (`src/kernel/inductive.cpp`,
`init_K_target`) for inductives that are "target for K-like reduction":
single (non-mutual, non-nested) inductive, in `Prop` (`is_zero`
result-level), with exactly one constructor -- `Eq`, `HEq`, `Acc`, `True`,
etc. all qualify; this is precisely the "subsingleton eliminator" shape,
so synthesizing an arbitrary same-typed inhabitant is sound (proof
irrelevance / the type has at most one canonical shape) specifically
because these preconditions hold, not more generally.

**Fix:** `iota_at_head`'s `Decl.Rec` case now takes `infer`/`isDefEq` as
extra parameters (threaded from `whnf_impl`'s call site, since `infer`,
`isDefEq`, and `whnf` are mutually recursive in `tc.ml`) and, when the
recursor's `is_K` flag is set, mirrors `to_cnstr_when_K` exactly before
computing `major_whnf`: infer+whnf the major premise's type, check its
head is a `Const` naming an `Inductive` decl with EXACTLY one constructor
that is also one of this recursor's own `rules` (guards against a
mismatched/foreign inductive), build `ctor_name` applied to the type's
first `num_params` arguments, infer that synthesized term's type, and
require `isDefEq` between the two types before accepting the substitution
-- falling back to the original (unreduced) major premise on any failure
at any step, exactly matching the kernel's own `return e` early-outs. The
speculative `isDefEq` check reuses the same logging-suppression pattern as
the existing `same_head_args_shortcut` (a failed attempt is a routine
"doesn't apply" outcome, not a real failure, and must not raise or spam
logs). This is a strict formalization of an existing-but-unused parser
field (`is_K`), not a new heuristic -- it fires only where the kernel
itself would, under the identical precondition (`is_K`, single-constructor
Prop inductive) and the identical final defeq safety check.

**Verification:** `NYAYA_ONLY_DECL=cast_eq` now passes. Regression: a full
plain discovery walk (`NYAYA_SKIP_PREFIX="Quot,Quotient" dune exec
bin/main.exe --root=.`, single process, one parse) ran past `cast_eq`
without stopping and covered 31628 declarations (43 skipped) before
hitting the next distinct failure (`StateT.run_modifyGet`, a separate,
not-yet-investigated structure-eta-shaped mismatch) -- confirms no
regression across everything checked along the way, a strictly larger
regression surface than the individually-listed prior fixes from earlier
entries in this changelog (all of which lie well before declaration
#31628 in hash order and were implicitly re-checked by this same walk).
**Kernel reference:** `src/kernel/inductive.h`'s `to_cnstr_when_K`,
`mk_nullary_cnstr`, `inductive_reduce_rec` (quoted above, fetched via
`raw.githubusercontent.com/leanprover/lean4/master/src/kernel/inductive.h`),
and `src/kernel/inductive.cpp`'s `init_K_target`/`mk_nullary_cnstr` (fetched
via `raw.githubusercontent.com/leanprover/lean4/master/src/kernel/inductive.cpp`).
**Declaration unblocked:** `cast_eq`

---

## 2026-07-10: Partial iota for Nat.mod's second equation (succ-headed dividend); unblocks Nat.mod.eq_2

### Nat.mod: one-step delta unfold for a `Nat.succ`-headed symbolic dividend
**File:** `tc.ml`, `nat_lit_reduce` (`Nat.mod` case)
**Problem:** `Nat.mod.eq_2` failed with `isDefEq: structural mismatch`
(`lhs = forall (h : LE.le ...), (fun _ => Nat) (Decidable.isTrue ... #0)`,
`rhs = Nat`). The equation lemma states
`Nat.mod (Nat.succ n) x = ite (LE.le x (Nat.succ n)) (Nat.modCore (Nat.succ n) x) (Nat.succ n)`,
provable by `rfl` because the RHS is exactly `Nat.mod`'s second defining
equation. Type-checking that `rfl` needs the LHS `Nat.mod (Nat.succ n) x`
to reduce to that same `ite`. But `Nat.mod` is (correctly) in
`is_nat_builtin_name`'s guarded set -- the guard blocks *all* delta on
`Nat.mod` so that whnf never unary-decrements a huge concrete dividend
through `modCore`'s well-founded recursion. With the O(1) literal path
unable to fire (`x` symbolic) and delta blocked, the LHS stayed stuck as
`Nat.mod (Nat.succ n) x` while the RHS was the explicit `ite`. The two
then fell into a positional structural-congruence comparison of a
`Nat.mod`-spine (2 args) against the unfolded `Decidable.rec`-spine
(5 args), which mismatched -- surfacing as the malformed
`Decidable.rec P motive minorFalse _` where a `Nat` free variable landed
in the `isTrue`-minor slot. Same *shape* of guarded-builtin gap that the
`Nat.pow`/`Nat.xor` entries handled: the guard has to stay, but the one
cheap step the real kernel gets for free needs an explicit escape hatch.

**Why one step is safe and bounded:** `Nat.mod`'s reference definition
(`Nat.mod`, `Init/Prelude.lean`) is
```lean
@[extern "lean_nat_mod"]
protected def Nat.mod : @& Nat → @& Nat → Nat
  | 0, _          => 0
  | n@(succ _), m => ite (LE.le m n) (Nat.modCore n m) n
```
The match is on the *dividend*. For a `Nat.succ`-headed dividend the
`succ`-branch fires in a single match-iota step, exposing the `ite`;
`Nat.modCore` sits inside the ite's then-branch (itself an `@[extern]`
well-founded def, and left unforced here since the `Decidable` instance
`Nat.decLe m n` is stuck for symbolic `m`), so no well-founded recursion
is entered. The `@[extern "lean_nat_mod"]` attribute is a compiler
directive only, irrelevant to kernel defeq; the kernel's own native mod
fast path (`type_checker.cpp`'s `reduce_nat`/`reduce_bin_nat_op`) requires
*both* operands to be concrete literals and, when that fails, falls
straight through to ordinary delta+iota -- exactly the one step reproduced
here. nyaya can't "fall through to delta" the way the C++ `whnf` loop does
(the guard exists precisely to stop that on concrete huge dividends), so
the safe one-step case needs its own hatch.

**Fix:** in `Nat.mod`'s `nat_lit_reduce` fallback, when the O(1) and
zero/ble rules don't fire, whnf the dividend; if it is `Nat.succ`-headed,
return one delta step of `Nat.mod` (`delta_at_head` on the bare head,
re-applied to the args) and let the recursive whnf perform the beta +
match-iota. Using `delta_at_head` reproduces `Nat.mod`'s real body rather
than hand-reconstructing the `ite`/`LE.le`/`Nat.decLe` subterms (which
would risk a subtly-wrong substitute, cf. the `Nat.xor` entry). This
fires *only* for a `Nat.succ`-headed dividend -- deliberately narrower
than the real match (which also takes the succ-branch for a positive
`NatLit`): a bare symbolic dividend or a positive literal stays stuck as
before, so no currently-passing `Nat.mod`-shaped declaration changes. A
narrower reduction can only lose completeness, never soundness (congruence
is preserved either way).

**Verification:** fast-tier (`Nat.mod.eq_2`, now passes). Regression:
re-ran individually and confirmed still passing:
`Nat.two_pow_succ`, `UInt32.not_neg_one`, `Fin.castSucc_one`,
`Nat.sub_one`, `Nat.add_succ_sub_one`, `Nat.lor.eq_1`, `Int.pred_toNat`,
`Int64.toInt32_ofBitVec`, `BitVec.replicate_zero`, `Fin.zero_le`.
**Kernel reference:** `src/Init/Prelude.lean`'s `Nat.mod`/`Nat.modCore`
definitions (fetched via
`raw.githubusercontent.com/leanprover/lean4/master/src/Init/Prelude.lean`),
and `src/kernel/type_checker.cpp`'s `reduce_nat`/`reduce_bin_nat_op` native
Nat fast path + `whnf` main loop.
**Declaration unblocked:** `Nat.mod.eq_2`

---

## 2026-07-10: Partial iota rule for Nat.pow with symbolic exponent; unblocks Nat.two_pow_succ

### Nat.pow: add a bounded partial-iota fallback, matching Nat.add/Nat.sub/Nat.mul
**File:** `tc.ml`, `nat_lit_reduce` (`Nat.pow` case)
**Problem:** `Nat.two_pow_succ` (`2^(n+1) = 2^n + 2^n`) failed with
`isDefEq: structural mismatch`. Its `rfl`-style proof term's own inferred
type states the LHS as `2^n * 2` (via `Nat.mul_two`), while the declared
type states it as `2^(n+1)` -- these are definitionally equal only via
`Nat.pow`'s own recursive equation (`pow m (n+1) = pow m n * m`), which
requires unfolding `Nat.pow` one step with the exponent `n+1` symbolic
(`n` a free variable). `Nat.pow` is (correctly) in `is_nat_builtin_name`'s
guarded set -- unlike `add`/`sub`/`mul`, though, it previously had no
partial-iota fallback for the guarded case, only the full O(1) literal
computation (`binary (fun m n -> Z.pow m n)`, requiring both base and
exponent to already be concrete literals). With a symbolic exponent this
fails, and since the guard also blocks plain delta-unfolding, `whnf` got
stuck at `Nat.pow 2 (n+1)` with no way to see the one step it needed.

**Why a single unfolding step here is safe (and citation for why the real
kernel does this too, not via `@[extern]`):** `Nat.pow`'s reference
definition (`Init/Prelude.lean`) is:
```lean
@[extern "lean_nat_pow"]
protected def Nat.pow (m : @& Nat) : (@& Nat) → Nat
  | 0      => 1
  | succ n => Nat.mul (Nat.pow m n) m
```
The `@[extern "lean_nat_pow"]` attribute is a *compiler* directive only --
it tells the code generator to call a native function when compiling/
running Lean programs, and has no bearing on kernel type-checking/defeq.
The kernel's actual native fast path for `Nat.pow` during type-checking is
a separate mechanism, `type_checker.cpp`'s `reduce_nat`/`reduce_pow`
(fetched via `raw.githubusercontent.com/leanprover/lean4/master/src/kernel/type_checker.cpp`):
```cpp
optional<expr> type_checker::reduce_pow(expr const & e) {
    expr arg1 = whnf(app_arg(app_fn(e)));
    if (!is_nat_lit_ext(arg1)) return none_expr();
    expr arg2 = whnf(app_arg(e));
    if (!is_nat_lit_ext(arg2)) return none_expr();
    nat v1 = get_nat_val(arg1);
    nat v2 = get_nat_val(arg2);
    if (v2 > nat(ReducePowMaxExp)) return none_expr();
    return some_expr(mk_lit(literal(nat(nat_pow(v1.raw(), v2.raw())))));
}
```
(`ReducePowMaxExp` is `1<<24` -- the native path even caps concrete-literal
exponents, falling through below that bound too.) This requires *both*
base and exponent to already be concrete literals -- same shape as nyaya's
existing `binary` fast path. Critically, when this fails (our case: `n+1`
symbolic), the kernel does NOT get stuck. Its `whnf` main loop:
```cpp
while (true) {
    expr t1 = whnf_core(t);
    if (auto v = reduce_native(env(), t1)) { ... }
    else if (auto v = reduce_nat(t1)) { ... }
    else if (auto next_t = unfold_definition(t1)) {   // falls through here
        t = *next_t;
    } else { ... }
}
```
falls straight through to ordinary delta-unfolding + iota, exactly as it
would for any other recursively-defined `Nat` function. This is cheap and
safe here specifically because `Nat.pow`'s definition is a plain `Nat.rec`
(structural recursion on the exponent) -- no `WellFounded.fix`/`Acc.rec`
accessibility-proof cost hidden inside it, unlike `Nat.bitwise` (see the
`Nat.xor` entry above). One iota step exposes one layer, unconditionally
cheap, regardless of how large the (symbolic) exponent eventually turns
out to be.

nyaya can't rely on "falls through to ordinary delta" the way the C++
kernel's `while` loop does, because nyaya's guard exists specifically to
block delta-unfolding `Nat.pow` on a *concrete* huge exponent one unary
step at a time (e.g. `2^4294967296`, which the real kernel instead computes
in one shot via GMP, capped at `1<<24`) -- removing the guard entirely
would reintroduce exactly the `Nat.xor`-style blowup for large concrete
exponents. So the guard has to stay, and the safe one-step case the real
kernel gets "for free" needs its own explicit escape hatch instead.

**Fix:** added a partial-iota fallback to `Nat.pow`'s `nat_lit_reduce` case,
matching the existing `Nat.add`/`Nat.sub`/`Nat.mul` pattern exactly:
```
Nat.pow m 0             → 1
Nat.pow m (NatLit n+1)  → Nat.mul (Nat.pow m (NatLit n)) m   [bounded, n ≤ 64]
Nat.pow m (Nat.succ y)  → Nat.mul (Nat.pow m y) m
```
The base `m` is never pattern-matched and may stay fully symbolic; only the
exponent is inspected, one step at a time, exactly mirroring `Nat.pow`'s
own recursive equation. `Nat.pow` remains in `is_nat_builtin_name`'s
guarded set, unchanged -- this only adds a bounded fallback path, it
doesn't touch the guard itself.

**Verification:** fast-tier (`Nat.two_pow_succ`, now passes). Checked for
an equation-lemma trap like the one found for `Nat.xor.eq_1`: `Nat.pow.eq_1`
and `Nat.pow.eq_2` don't exist in `init.export` at all, and `Nat.pow_succ`,
`Nat.pow_zero`, `Nat.pow_succ'` all still pass. Regression-checked
individually via `NYAYA_ONLY_DECL` against all declarations currently
listed elsewhere in this changelog as "Declaration unblocked".
**Kernel reference:** Lean 4's `src/kernel/type_checker.cpp`,
`reduce_pow`/`reduce_bin_nat_op`/`whnf`'s main loop (quoted above, fetched
via `raw.githubusercontent.com/leanprover/lean4/master/src/kernel/type_checker.cpp`),
and `src/Init/Prelude.lean`'s `Nat.pow` definition (fetched via
`raw.githubusercontent.com/leanprover/lean4/master/src/Init/Prelude.lean`).
**Declaration unblocked:** `Nat.two_pow_succ`

---

## 2026-07-10: Guard Nat.xor against Nat.bitwise well-founded-recursion blowup; unblocks UInt32.not_neg_one

### Nat.xor: don't delta-unfold into Nat.bitwise's well-founded-recursion body
**File:** `tc.ml`, `is_nat_builtin_name`, `nat_lit_reduce` (`Nat.xor` case),
`isDefEq_impl` (last-resort `Nat.xor` one-step rule)
**Problem:** `UInt32.not_neg_one` (`~(-1 : UInt32) = 0`, proved by `rfl`)
failed with `Depth_limit("[w#4593] depth 2001 exceeds max 2000,
input=Nat.eq_or_lt_of_le.match_1 2147483451 4294967098 ...")`. This is a
different bug *class* from most prior entries: not a missing whnf/iota rule
causing a false mismatch, but a genuine reduction-termination bug. Traced
(via a diagnostic rebuild with the `same_head_args_shortcut`'s speculative
logging temporarily un-silenced, reverted before this fix) to: `UInt32.
complement` routes through `BitVec.not` to `Nat.xor`, whose reference
definition is `Nat.xor = Nat.bitwise bne`; `Nat.bitwise` is implemented via
`Nat.bitwise._unary`, built on `WellFounded.fix`/`Acc.rec`, whose
*accessibility proof* is resolved via ordinary structural `Nat` recursion
(through `Nat.eq_or_lt_of_le`-style comparison lemmas, themselves defined
via `Nat.brecOn`) applied to the concrete ~2^31/2^32-scale operands --
genuinely O(n) in the operand *value* when forced by kernel iota, not
O(log n) as the removed comment on `is_nat_builtin_name` had assumed for
all five bitwise ops. Confirmed via nyaya's own `NYAYA_DECL_DEBUG` dump of
`Nat.eq_or_lt_of_le`'s value: it's `Nat.brecOn` structural recursion on one
of the two compared naturals, with no O(1) shortcut -- descending from a
~2^31-scale literal to 0 one `Nat.succ` layer at a time is what blew the
depth limit. The existing `is_nat_builtin_name` guard (already correctly
preventing this class of blowup for `add`/`sub`/`mul`/`div`/`mod`/`beq`/
`ble`) explicitly did *not* cover the bitwise operations, on the
(incorrect) assumption that `Nat.bitwise`'s divide-by-2 recursion made
delta-unfolding it safe -- that assumption ignored the *accessibility
proof*'s own unrelated-and-expensive reduction cost.
**Fix:** Add `Nat.xor` and `Nat.bitwise` to `is_nat_builtin_name`'s guarded
set, so `whnf` stops at `Nat.xor`/`Nat.bitwise` rather than delta-unfolding
further when `nat_lit_reduce`'s O(1) literal computation doesn't fire. This
forces argument resolution back through the existing recursive
`whnf`/`nat_lit_reduce` path (`BitVec`/`Fin` projections, `Nat.sub`/
`Nat.pow`, all already guarded and working) -- sufficient for
`UInt32.not_neg_one`'s fully concrete operands, and a strictly *more*
faithful reduction (matching the real kernel, which never delta-unfolds
these either), not a shortcut that skips any comparison.

`Nat.land`, `Nat.lor`, `Nat.shiftLeft`, `Nat.shiftRight`, `Nat.testBit` are
deliberately left UNGUARDED, unchanged -- narrower than the citation would
in principle justify, but empirically necessary: guarding an outer name
here blocks *all* delta on it, including the single unconditional delta
step some already-passing declaration needs (an equation lemma stating,
e.g., `Nat.lor = Nat.bitwise Bool.or` point-free, with no arguments ever
applied to trigger a narrower rule). Verified this risk directly rather
than assuming it away: reproduced `Nat.lor.eq_1` (documented as unblocked
by *removing* bitwise ops from this exact guard, 2026-07-08 entry further
down) as an actual regression when guarding `Nat.land`/`Nat.lor`/
`Nat.xor`/`Nat.shiftLeft`/`Nat.shiftRight` together, then checked the
sibling declarations one at a time and found three more of the same shape
already passing and equally at risk: `Nat.land.eq_1`, `Nat.shiftLeft.eq_1`,
`Nat.shiftLeft.eq_2` (`Nat.shiftRight` has no such lemma in `init.export`).
`Nat.xor.eq_1` needs the identical one-step exposure `Nat.lor.eq_1` does,
so guarding `Nat.xor` needed its own fix, not just omission:
- `nat_lit_reduce`'s `Nat.xor` case: when the O(1) literal computation
  fails but both arguments are present, produce the one-step delta unfold
  (`Nat.xor n m → Nat.bitwise bne n m`) via `delta_at_head` on the bare
  head, rather than leaving the application stuck.
- `isDefEq_impl`: a last-resort structural rule, alongside the existing
  struct-eta/lambda-eta fallbacks, that tries the same one-step unfold
  when one side of an otherwise-stuck comparison is literally the bare
  `Nat.xor` constant -- needed because `Nat.xor.eq_1` states its equation
  point-free (no arguments anywhere for the `nat_lit_reduce` case above to
  ever see).

Both use `delta_at_head` to look up `Nat.xor`'s real declaration value
programmatically (the same thing ordinary delta-unfolding would produce),
rather than hand-reconstructing the `bne`/`instBEqOfDecidableEq` subterm,
which would risk a subtly-wrong hand-written substitute. `Nat.land`/
`Nat.lor`/`Nat.shiftLeft`/`Nat.shiftRight` get no equivalent guard or
one-step exception in this fix -- they are not implicated in the bug being
fixed (`UInt32.not_neg_one`'s reduction only reaches `Nat.xor`), and
extending the guard to them speculatively, without a concrete failing
declaration driving it, would mean inventing their one-step-exception
mechanisms (`Nat.shiftLeft`/`Nat.shiftRight` in particular have no
`Nat.bitwise`-shaped one-liner to unfold to at all -- their reference
definition really is the `Nat.brecOn` recursion itself) without the
verification a real failing case provides. If one of them is later found
to cause the same class of blowup, it should get the same targeted
treatment then, backed by that declaration's own citation.

**Verification:** fast-tier (`UInt32.not_neg_one`, now passes, no longer
needs anywhere near the depth limit) plus a deliberately widened regression
check: all 16 declarations currently listed as "Declaration unblocked"
elsewhere in this changelog (`Vector.pmap_attach`,
`Int64.toInt32_ofBitVec`, `Nat.sub_one`, `Fin.castSucc_one`, `Fin.zero_le`,
`String.length_singleton`, `Nat.lor.eq_1`, `List.id_run_foldlM`,
`Nat.add_succ_sub_one`, `BitVec.replicate_zero`, `List.get_cons_succ'`,
`ISize.not_xor`, `Vector.findM?_pure`, `BitVec.not_sshiftRight`,
`Int.pred_toNat`, `Float32.toUInt32`), plus the four additional
newly-identified at-risk declarations found while investigating this fix
specifically (`Nat.xor.eq_1`, `Nat.land.eq_1`, `Nat.shiftLeft.eq_1`,
`Nat.shiftLeft.eq_2`) -- all 21 declarations checked individually via
`NYAYA_ONLY_DECL`, all still pass.
**Kernel reference:** Lean 4's `src/kernel/type_checker.cpp`,
`reduce_nat()`'s dispatch table (fetched via
`raw.githubusercontent.com/leanprover/lean4/master/src/kernel/type_checker.cpp`):
```cpp
if (f == *g_nat_add) return reduce_bin_nat_op(nat_add, e);
if (f == *g_nat_sub) return reduce_bin_nat_op(nat_sub, e);
if (f == *g_nat_mul) return reduce_bin_nat_op(nat_mul, e);
if (f == *g_nat_pow) return reduce_pow(e);
if (f == *g_nat_gcd) return reduce_bin_nat_op(nat_gcd, e);
if (f == *g_nat_mod) return reduce_bin_nat_op(nat_mod, e);
if (f == *g_nat_div) return reduce_bin_nat_op(nat_div, e);
if (f == *g_nat_beq) return reduce_bin_nat_pred(nat_eq, e);
if (f == *g_nat_ble) return reduce_bin_nat_pred(nat_le, e);
if (f == *g_nat_land) return reduce_bin_nat_op(nat_land, e);
if (f == *g_nat_lor)  return reduce_bin_nat_op(nat_lor, e);
if (f == *g_nat_xor)  return reduce_bin_nat_op(nat_lxor, e);
if (f == *g_nat_shiftLeft) return reduce_bin_nat_op(lean_nat_shiftl, e);
if (f == *g_nat_shiftRight) return reduce_bin_nat_op(lean_nat_shiftr, e);
```
The real kernel computes `Nat.land`/`Nat.lor`/`Nat.xor`/`Nat.shiftLeft`/
`Nat.shiftRight` natively via GMP bit operations, alongside
`add`/`sub`/`mul`/`pow`/`gcd`/`mod`/`div`/`beq`/`ble` -- it never
delta-unfolds any of their Lean-level reference definitions
(`Nat.bitwise`'s well-founded recursion, or `Nat.shiftLeft`/
`Nat.shiftRight`'s own `Nat.brecOn` recursion). This fix restores that
faithfulness for `Nat.xor` specifically, the operation actually exercised
by `UInt32.not_neg_one`. `Nat.testBit` has no entry in this dispatch table
(no native kernel reduction rule), so leaving it unguarded doesn't diverge
from the real kernel's behavior for it either way.
**Declaration unblocked:** `UInt32.not_neg_one`

---

## 2026-07-09: Two real bugs found and fixed on Vector.pmap_attach — one OOM, one non-termination

**Status: this is a real fix for a real declaration, and likely a major
contributor to the OOM wall described in the entry below (same day, earlier).
Not yet re-run against the full sweep to confirm the wall itself is gone —
see "Next" at the end.**

Investigating a soft failure where `Vector.pmap_attach` OOM-killed the process
under `NYAYA_ONLY_DECL`, two independent, stacked bugs were found by
instrumenting call counts and GC stats and running under `ulimit -v` to get a
fast, repeatable crash instead of eating all system memory.

### Debugging workflow (reusable recipe for the next OOM/hang-shaped bug)

The two bugs below were found without ever letting the process actually
exhaust system memory — that's slow (minutes) and destabilizes the whole
machine each time. Instead:

**1. Get the exact `NYAYA_ONLY_DECL` name first.** It matches the full
dotted name from `Name.pp`, not the bare identifier — `grep`ping the raw
`.export` file for the identifier only gives you the unqualified form:
```sh
grep -n "pmap_attach" test/parser/init.export   # confirms it exists, wrong form for NYAYA_ONLY_DECL
NYAYA_ONLY_DECL="Vector.pmap_attach" timeout 90 dune exec bin/main.exe  # correct: Namespace.decl
```

**2. Cap virtual memory with `ulimit -v` (KB) in a subshell**, so a runaway
process dies in seconds with `Out_of_memory` instead of slowly eating all
system RAM+swap. The parens make it a subshell so the limit doesn't leak
into your interactive shell. Always redirect both streams to a log file and
keep `timeout` as a second, independent backstop (`ulimit` doesn't help a
process that hangs without allocating):
```sh
(ulimit -v 3000000; NYAYA_ONLY_DECL="Vector.pmap_attach" timeout 60 dune exec bin/main.exe) \
  > /tmp/run.log 2>&1
echo "EXIT:$?"
tail -n 80 /tmp/run.log
```
Try the cap at a couple of different sizes (e.g. 3_000_000 then 10_000_000,
i.e. ~3GB then ~10GB) with the *same* command. If it dies at the same point
in the code regardless of the cap, that's a strong signal it's either (a) a
single catastrophic allocation, or (b) a hang with flat memory (an infinite
or exponential-time loop that never allocates much) — not "genuinely needs
N GB of legitimate working set." That distinction is exactly what separated
bug 1 (real, fast, huge allocation) from bug 2 (a hang, not a leak) below.

**3. Instrument suspects directly in source with cheap global counters +
periodic `Gc.quick_stat()`, flushed unconditionally.** The pattern used
(then removed once the bugs were found — don't leave this in the tree):
```ocaml
let calls = ref 0
...
incr calls;
if !calls mod 5_000 = 0 then (
  let gc = Gc.quick_stat () in
  Printf.eprintf "[DBG %s] calls=%d live_words=%d heap_words=%d\n%!"
    label !calls gc.live_words gc.heap_words
)
```
Two details matter: the `%!` (force-flush every line — under `timeout`/
`ulimit` the process can be killed before a buffered `Format`/`Printf`
channel would otherwise flush, silently losing the last N seconds of
signal) and picking the modulus small enough to actually get a data point
before the crash (started at 200_000, had to drop to 5_000/20_000 once the
first runs printed *nothing* before dying — that itself was informative,
see below). Wire a reset for each new counter into `Tc.typecheck`'s
existing per-declaration reset block (next to `Hashtbl.reset whnf_memo`
etc.) so a `NYAYA_ONLY_DECL` run starts every counter at zero.

**4. Read the *shape* of the counter output to distinguish failure modes,
don't just look at whether it crashed:**
- **Nothing printed before death, even at a low modulus and a high memory
  cap** → not many-small-allocations; look for a single expensive call
  very early (this is how bug 1 — an eager `Expr.pp` inside a trace
  logger — was found: the crash happened between two adjacent counter
  checkpoints that were only a few hundred calls apart).
- **A counter climbs into the billions while `live_words`/`heap_words`
  stay flat** → not a memory problem at all, it's non-termination (a loop
  or exponential recursion that does negligible allocation per step). This
  is how bug 2 was distinguished from "just needs more memory" — before
  the fix, `num_loose_bvars`'s counter passed 9 billion with `live_words`
  unchanged across dozens of printed checkpoints, i.e. actually hung, not
  leaking.

**5. Regression-check any fix on the substitution/typechecking hot path
against a handful of previously-passing declarations**, not just the one
you were debugging — cheap because `NYAYA_ONLY_DECL` gives a fresh,
independent process per name:
```sh
for d in "Int64.toInt32_ofBitVec" "Nat.sub_one" "Fin.castSucc_one" \
         "Fin.zero_le" "String.length_singleton"; do
  echo "=== $d ==="
  timeout 90 env NYAYA_ONLY_DECL="$d" dune exec bin/main.exe 2>&1 | tail -4
done
```
(Names above are just examples pulled from earlier entries in this
changelog — swap in whichever declarations were confirmed passing most
recently.) Note baseline overhead: parsing `init.export` + building the
environment takes ~20s on its own before any checking starts, so size
`timeout` accordingly or a passing declaration will look like a hang.

### Bug 1: `MakeTrace.enter`/`leave_success`/`leave_failure` eagerly computed
debug-only pretty-prints on every call, regardless of log level

**File:** `lib/parser/util.ml`, `MakeTrace.enter`/`leave_success`/`leave_failure`

**Problem:** these three functions called
`Logger.debug "...%s..." ... (Data.input_summary input)` — passing
`Data.input_summary input` (for `InferTrace`/`WhnfTrace`, `expr_summary` =
a full, non-memoized `Expr.pp` tree-walk of the expr) as a plain, positional
function argument. OCaml evaluates function arguments eagerly, so this
pretty-print ran unconditionally on **every single `infer`/`whnf` call**
(enter and leave), even with logging at `Info` level and no debug output
ever shown. `Logs.debug (fun m -> ...)`'s own level check only happens
*inside* the closure passed to it — by the time that closure would run, the
expensive `%s` argument had already been fully computed to build the format
string. `Expr.pp` walks the hash-consed DAG as if it were a tree (no
memoization on `tag`), so on a term with heavy internal sharing this is not
just wasted work but potentially an exponential-length string built in a
single allocation. Confirmed by observing the process die instantly (before
any of several independently-added call counters advanced meaningfully) even
at a 10GB `ulimit -v`.

**Fix:** gate all three call sites behind an explicit
`Logs.level () = Some Logs.Debug` check, so `input_summary`/`output_summary`
are only ever computed when debug output will actually be printed.

**Why this matters beyond this one declaration:** this ran on every
`infer`/`whnf` enter and leave for *every* declaration checked, in every run
configuration (`NYAYA_SWEEP_ALL`, plain discovery, `NYAYA_ONLY_DECL`) — a
low-grade firehose of transient string allocation across the whole corpus.
This is a plausible (not yet confirmed) contributor to the "steady growth, no
single blowup point" OOM signature from the entry below.

### Bug 2: `num_loose_bvars` re-walks the DAG as a tree on every call from `instantiate`

**File:** `lib/expr.ml`, `num_loose_bvars` / `instantiate`

**Problem:** `instantiate_aux` (the beta/let/pi-body substitution engine
used by essentially every case of `infer`/`whnf`/`isDefEq`) calls
`num_loose_bvars expr` at every recursion step, as a short-circuit ("does
this subtree even contain the bound var we're substituting?"). This TODO was
already flagged in the source (`"this needs to be optimized by counting the
number of loose bound variables in the expr"`), but the actual failure mode
wasn't confirmed until now: `num_loose_bvars` itself is a full recursive
tree-walk with **no memoization**, called on a hash-consed DAG with heavy
internal sharing (a well-founded-recursion-generated proof term, in this
case). After fixing bug 1 (which had been OOMing the process before this
code path could even run long enough to matter), `num_loose_bvars` call
counts were observed climbing past **9 billion with zero further progress**
and flat memory (i.e. genuinely non-terminating within a 90s window, not
merely slow) — a DAG-as-tree exponential re-walk, not a memory leak.

**Fix:** memoize `num_loose_bvars` by hash-cons `tag` in a new
`Expr.num_loose_bvars_memo : (int, int) Hashtbl.t`. This is sound
unconditionally (no per-declaration reset needed for correctness): the
function is a pure structural property of the expr node, independent of
`env`/typechecking state/offset, and a given tag's underlying node never
changes. It's reset per declaration anyway (alongside `whnf_memo`/
`infer_memo` in `Tc.typecheck`, all three call sites), purely to bound total
memory across a long sweep, matching the existing convention for those two
tables — not because the cache can go stale.

**Result:** `Vector.pmap_attach` now type-checks successfully in ~60s
(previously: non-terminating / OOM). Verified no regression on 5 known-passing
declarations from earlier entries in this changelog (`Int64.toInt32_ofBitVec`,
`Nat.sub_one`, `Fin.castSucc_one`, `Fin.zero_le`, `String.length_singleton`) —
all still pass.

**Declaration unblocked:** `Vector.pmap_attach` (was OOM/non-terminating, not
a `Defeq_failure`/`TypeError`, so it wouldn't have shown up as a normal
"failing declaration" — it would have just killed whatever process reached
it).

**Next:** ~60s for one declaration (with ~200M `instantiate_aux` calls
remaining, per the advisor's read of the diagnostic data) means `instantiate`
itself is still doing a lot of redundant DAG-as-tree work — a further
optimization is possible (e.g. memoizing `instantiate` keyed on
`(tag, offset)`, not just `tag`, since substitution result depends on
`offset` too) but was deliberately not attempted this session: the reported
problem (OOM/hang) is fixed, and that's a separate, riskier piece of work.
Not yet re-run: the full `NYAYA_SWEEP_ALL` corpus sweep, to see whether these
two fixes (especially bug 1, which fired on every declaration in every mode)
move or remove the ~9000-declaration OOM wall from the entry below. That's
the natural next step for whoever picks up `project_nyaya_oom_investigation`
next.

---

## 2026-07-09: Discovery blocked by the OOM wall — loop paused, OOM promoted to its own investigation

**Status: loop paused here. This is the handoff point for whoever picks up next.**

After the `isDefEq` same-head shortcut landed (next entry below, `99db72a`),
the standard discovery step — plain `dune exec --root=. nyaya`, no env vars,
which walks the declaration hashtable and is supposed to stop at the first
failure — was run to find the next target. Result: it processed **9478
declarations, all passing, zero failures found**, before being killed with
`EXIT=137` (SIGKILL, i.e. the OS OOM-killer). Re-running it deterministically
re-hits the same wall for zero new information, since the hashtable walk
order is fixed and nothing about the crash depends on which declaration it
happens to land on.

**This is not a new bug.** It's the same open issue in
`project_nyaya_oom_investigation` (Claude's persistent memory) /
this session's earlier finding: a `NYAYA_SWEEP_ALL` full-corpus sweep was
separately OOM-killed after ~69 minutes at ~14.5GB RSS, ~9000+ declarations
in, steady growth, no single blowup point. **The two numbers lining up
(~9000-9500 declarations before SIGKILL, in two totally different run
configurations) is itself informative**: it suggests the crash point is
governed by cumulative memory used per declaration processed within a single
long-running process, not by which specific declaration is being checked or
by which env-var mode is active. Concretely, this means:
- A one-time `NYAYA_SWEEP_ALL` survey (the plan's designated "run this once,
  right before merge, to get full visibility" tool) will almost certainly
  hit the *same* wall around the *same* declaration count, for the same
  reason a second plain-discovery run would — it does not fix or route
  around per-process memory accumulation, it only continues past *caught
  exceptions* (and explicitly does not catch `Out_of_memory`/`Stack_overflow`
  in the first place, per its original design intent).
- Any strategy that keeps processing declarations one after another inside
  a single OCaml process is capped at roughly this same ceiling until the
  actual root cause is found and fixed. Restarting the process between
  declarations (what `NYAYA_ONLY_DECL` does, one fresh process per
  declaration) sidesteps the problem rather than fixing it, and doesn't
  scale to "find me the next failure" when you don't already know its name.

**Ruled out already** (verified by reading the actual code, not assumed —
see `project_nyaya_oom_investigation` memory for the full detail):
- Hash-cons table (`lib/expr.ml`, `HExpr = Hc.Make (E)`) genuinely uses
  `Ephemeron.K1.Make` (confirmed via `~/.opam/5.1.0/lib/hc/hc.ml`) — a real
  weak-key GC-collectible table. The stale TODO comment in `expr.ml` about
  wanting Ephemerons is describing something already done; don't re-blame
  this.
- `MakeTrace.reset` and `whnf_memo`/`infer_memo` are all correctly cleared
  per declaration — not accumulation sources.

**Not yet checked / where to start next:**
- Whether OCaml's major GC is actually keeping pace with a long sustained
  allocation rate (ephemeron clearing only happens during major GC sweeps;
  default `space_overhead` tuning could produce large peak RSS under high
  sustained allocation even with no true "leak"). Try `OCAMLRUNPARAM` GC
  stats during a long run, or test whether periodic `Gc.compact ()` changes
  RSS behavior over ~5000+ declarations.
- Any other module-level cache not yet audited for eviction (e.g. does
  `Level.simplify` or anything in `lib/name.ml` memoize globally without
  ever clearing?).
- Whether per-node representation overhead (boxing, record fields, the
  hash-consed wrapper) is just inherently heavier than the C++ kernel's
  representation, independent of any leak — baseline cost per node, not
  accumulation over time. If so, the fix is a representation change, not a
  GC-tuning fix.
- A profiler run (e.g. `perf`, or OCaml's own `landmarks`/`memtrace`) across
  a multi-thousand-declaration run would likely settle this faster than more
  code reading — this hasn't been tried yet.

**Decision (Kody, 2026-07-09):** the OOM issue is promoted from "open,
revisit later" to "open, dedicated investigation, decoupled from the
correctness loop." It's a bigger, more open-ended engineering problem
(profiling, GC tuning, possibly a representation change) than the
one-declaration-at-a-time citation-gated fixes this loop was scoped for, and
it's now the actual blocker on making further correctness progress via the
discovery mechanism (not just a performance nice-to-have). A new session
should pick this up on its own, separate from the debug-fix loop
infrastructure described in `doc/agentic-loop-retrospective.md`.

**To resume the correctness loop without waiting on the OOM fix:** discovery
doesn't have to be a full-corpus walk. A `NYAYA_ONLY_DECL=<name>` check is
still cheap and memory-bounded for any *specific* candidate name — the
constraint is only that nothing currently provides such a name past
`Int64.toInt32_ofBitVec` (the last one found before the wall). Manually
sampling declaration names from `init.export` (e.g. scanning for names
alphabetically/structurally near recent fixes, or picking from a category
not yet exercised) and trying them one at a time via `NYAYA_ONLY_DECL` would
work without touching the OOM problem, just less systematically than
"run discovery and see what it finds."

---

## 2026-07-09: Add lazy-delta-reduction "same head" shortcut to isDefEq; unblocks Int64.toInt32_ofBitVec

### isDefEq: compare same-head applications' arguments before delta-unfolding
**File:** `tc.ml`, `isDefEq_impl`
**Problem:** `Int64.toInt32_ofBitVec` (an `rfl`-proved theorem relating
`Int64.toInt32 (Int64.ofBitVec b)` to `Int32.ofBitVec (BitVec.signExtend 64
32 b)`) hit `DEPTH LIMIT: ... input=Nat.sub (BitVec.toNat 64
(Int64.toBitVec (Int64.ofBitVec b))) 18446744073709551117` (a ~2^64
literal). Root cause, found by reading the full debug trace: our
`isDefEq_impl` always calls `whnf` on both sides of a comparison before any
structural check. When both sides of a comparison share a head like
`BitVec.signExtend` (same declaration, applied to different BitVec
arguments on each side), `whnf` unconditionally delta-unfolds that shared
head all the way down into its full computational body (through
`BitVec.ofInt`, `BitVec.ofNatLT`, `Int.toNat`, `HMod.hMod`, eventually
`Nat.sub` on a raw Nat encoding) *before* the two sides' actual differing
argument (`Int64.toBitVec (Int64.ofBitVec b)` vs `b` -- which reduces to
each other in two cheap constructor/projection steps) is ever compared
directly. The expensive, symbolic-minuend Nat.sub this produces is the
thing that exceeded the depth limit; it need never have been computed at
all.
**Fix:** Added `same_head_args_shortcut`, tried after the existing
proof-irrelevance check and before `whnf`: if both sides (pre-whnf) are
applications of the exact same declaration (same `Const` name, same
universe params) with equal arity, compare their arguments pairwise via
`isDefEq` first; if all pairs succeed, the whole application is equal
(sound by congruence: `aᵢ ≡ bᵢ` for all `i` implies `f a⃗ ≡ f b⃗`, so this can
only ever be a true positive, never a false one). If the shortcut doesn't
apply, or any argument pair fails, execution falls through completely
unchanged to the existing whnf-based algorithm -- exactly as if the
shortcut were absent. One subtlety caught during regression testing: our
`isDefEq`'s own final "structural mismatch" fallback does not return
`false` -- it *raises* `Defeq_failure` (via `Logger.err`), uncaught by the
`isDefEq` wrapper. The shortcut's pairwise argument comparison must catch
that exception and treat it as "argument pair failed" (i.e. fall through),
not let it escape and abort the whole outer comparison -- otherwise a
nested mismatch reachable only via the shortcut's *un-reduced* argument
comparison could break declarations that used to pass via the *reduced*
(whnf'd) comparison path. This was caught by regression-testing all 14
previously-unblocked declarations before committing (see Verification
below) -- `Fin.zero_le` and `List.get_cons_succ'` initially regressed with
exactly this failure mode, fixed by wrapping the pairwise check in `try
... with Defeq_failure _ -> false`.
**Kernel reference:** `type_checker.cpp` (leanprover/lean4,
`src/kernel/type_checker.cpp`), fetched via
`raw.githubusercontent.com/leanprover/lean4/master/src/kernel/type_checker.cpp`
and verified against the raw fetched text directly. The relevant functions:
`is_def_eq_app` (the argument-wise comparison for two applications) and
`lazy_delta_reduction_step`, which gates entry into that argument-wise
comparison specifically for two applications delta-headed by the *same*
declaration with matching universe levels, tried *before* delta-unfolding
that shared declaration:
```cpp
bool type_checker::is_def_eq_app(expr const & t, expr const & s) {
    if (is_app(t) && is_app(s)) {
        buffer<expr> t_args;
        buffer<expr> s_args;
        expr t_fn = get_app_args(t, t_args);
        expr s_fn = get_app_args(s, s_args);
        if (is_def_eq(t_fn, s_fn) && t_args.size() == s_args.size()) {
            unsigned i = 0;
            for (; i < t_args.size(); i++) {
                if (!is_def_eq(t_args[i], s_args[i]))
                    break;
            }
            if (i == t_args.size())
                return true;
        }
    }
    return false;
}
```
and, inside `lazy_delta_reduction_step`, the same-declaration/same-height
case:
```cpp
if (is_app(t_n) && is_app(s_n) && is_eqp(*d_t, *d_s) && d_t->get_hints().is_regular()) {
    // Optimization: We try to check if their arguments are definitionally
    // equal. If they are, then t_n and s_n must be definitionally equal,
    // and we can skip the delta-reduction step.
    if (!failed_before(t_n, s_n)) {
        if (is_def_eq(const_levels(get_app_fn(t_n)), const_levels(get_app_fn(s_n))) &&
            is_def_eq_args(t_n, s_n)) {
            return reduction_status::DefEqual;
        } else {
            cache_failure(t_n, s_n);
        }
    }
}
```
Our version does not replicate `d_t->get_hints().is_regular()` (a
performance heuristic restricting the kernel's version to plain,
non-reducible/non-irreducible definitions) or the `failed_before`/
`cache_failure` memoization (a performance optimization, not a correctness
requirement) -- omitting both only means we attempt the shortcut on a
superset of the declarations the kernel would bother trying it on, or
retry a failed comparison instead of caching it, neither of which affects
soundness (the congruence argument holds regardless of which declarations
we try it on, and retrying a failed check just costs time, not
correctness).
**Verification:** Fast-tier (`NYAYA_ONLY_DECL=Int64.toInt32_ofBitVec`)
confirms the target now passes. Additionally ran a full regression pass
(fresh process per declaration) over every declaration listed as
"Declaration unblocked" earlier in this changelog -- `Nat.sub_one`,
`Int.pred_toNat`, `Fin.castSucc_one`, `Fin.zero_le`,
`String.length_singleton`, `Nat.lor.eq_1`, `List.id_run_foldlM`,
`Nat.add_succ_sub_one`, `BitVec.replicate_zero`, `List.get_cons_succ'`,
`ISize.not_xor`, `Vector.findM?_pure`, `BitVec.not_sshiftRight`,
`Float32.toUInt32` -- all still pass after the fix (see the `Defeq_failure`
catch above; the first attempt without that catch regressed `Fin.zero_le`
and `List.get_cons_succ'`, caught and fixed before this commit).
**Declaration unblocked:** `Int64.toInt32_ofBitVec`

---

## 2026-07-09: Considered and reverted — bounding Nat.sub's literal-subtrahend partial iota to k ≤ 64

**Not committed.** A follow-up to the `Nat.sub` faithful single-step rule
(previous entry, `50af38e`) proposed bounding the concrete-subtrahend case
(`Nat.sub a (NatLit k)` with `a` symbolic) to `k ≤ 64`, mirroring the existing
`Nat.add`/`Nat.mul` literal-successor bounds a few lines below it in
`nat_lit_reduce`, to avoid peeling a huge concrete literal (e.g. a
`2^64`-ish BitVec/UInt mask) one `Nat.pred` at a time.

**Why this is off the SAFE/RISKY axis, not SAFE:** the plan's SAFE label
means "reproduces a documented kernel rule with the kernel's own
preconditions." The real kernel's `Nat.sub` has no such cutoff — it's
`@[extern]`, and with a symbolic minuend and literal subtrahend it iota-
reduces by peeling `succ` with no bound. This proposed change is *narrower*
than the kernel, not a faithful reproduction of an additional kernel
precondition, so it doesn't earn a `Kernel reference:` field. It also can't
be unsound: a bound that only makes whnf get stuck *more* often can never
make two non-defeq terms compare equal — narrowing is soundness-safe by
construction (same reasoning as why the `Nat.add`/`Nat.mul` bounds already
in the file are fine).

**Why reverted anyway:** (1) it doesn't unblock any declaration — no
`debug_*.txt` motivated it, it was proposed speculatively; (2) it is not
verifiable by the fast tier — no currently-failing declaration exercises a
literal subtrahend > 64, so there is no way to confirm the cutoff value or
even that the branch is reachable; (3) the runaway case it targets (huge
literal, symbolic minuend) is already caught by the existing recursion/depth
guard, just with a different failure mode (`Depth_limit` exception vs. a
stuck term) — so the change trades one already-handled failure mode for
another, for zero net gain; (4) `k ≤ 64` lands squarely in BitVec/UInt mask
territory, i.e. exactly the range where getting the cutoff wrong would be
most likely to matter, with no test to catch a wrong choice. Verdict: revert
until a real failing declaration actually hits this path, then add the bound
with a concrete case to verify against.

---

## 2026-07-08: Fix unsound Nat.sub partial iota; unblocks Nat.sub_one

### Nat.sub: replace paired-decrement shortcut with faithful single-step rule
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** `Nat.sub_one` (`n - 1 = Nat.pred n`, proved by `rfl`) failed with a
structural mismatch bottoming out at `Nat.sub n##83 =?= Nat.rec ...` (comparing
`Nat.sub`'s and `Nat.pred`'s delta-unfolded recursor forms directly, rather
than reducing `Nat.sub n##83 1` down to `Nat.pred n##83` first). Investigating
this surfaced a deeper, pre-existing problem: the *existing* partial-iota
rules for `Nat.sub` — `Nat.sub 0 _ → 0` and `Nat.sub (succ n) (succ m) →
Nat.sub n m` (added 2026-03-26, before this session's citation discipline) —
do not match the real kernel's single-step reduction and are only
*propositionally* true (provable by induction on the second argument), not
*definitionally* true by iota alone, when that argument is symbolic. E.g. the
real `Nat.sub 0 (succ b)` iota-reduces to `Nat.pred (Nat.sub 0 b)`, which is
*stuck* when `b` is a free variable — not the literal `0` the old rule
claimed. This is the same class of over-generalization flagged in
`doc/soundness-risks.md` entry #1: a rule that's true for the cases it was
built to unblock, generalized further than the cited definition supports.
**Fix:** Replaced both special cases with a single rule that mirrors the real
definition's one-step reduction exactly: `Nat.sub a 0 → a` (unchanged, this
one *is* the literal first match arm) and `Nat.sub a (succ b) → Nat.pred
(Nat.sub a b)` — recursing only on the second argument, leaving the first
argument `a` untouched and possibly fully symbolic, then wrapping in
`Nat.pred` (not a kernel builtin, so it delta-unfolds and iota-reduces
normally once its own argument becomes constructor-headed). This is what
directly resolves `Nat.sub_one`: `Nat.sub n 1 → Nat.pred (Nat.sub n 0) →
Nat.pred n` via the existing `Nat.sub a 0 → a` case plus this new one,
landing on exactly the theorem's RHS.
**Kernel reference:** `Nat.sub`, `src/Init/Prelude.lean` (leanprover/lean4),
fetched via `cdn.jsdelivr.net/gh/leanprover/lean4@master/...` (raw GitHub
mirror), verified against the raw fetched text directly:
```
@[extern "lean_nat_sub", instance_reducible]
protected def Nat.sub : (@& Nat) → (@& Nat) → Nat
  | a, 0      => a
  | a, succ b => pred (Nat.sub a b)
```
Pattern matching is on the *second* argument only; the first argument is
never inspected by the definition, so no rule may require it to be
succ-decomposable, and no rule may claim a result without wrapping in
`Nat.pred` per recursive step.
**Regression note:** the removed `Nat.sub (succ n) (succ m) → Nat.sub n m`
case previously unblocked `Int.pred_toNat` (2026-03-26 entry). Re-verified
`Int.pred_toNat` still passes with the corrected rule (fast-tier,
`NYAYA_ONLY_DECL=Int.pred_toNat`) — its use case only ever had a concrete
literal in the relevant position, so the faithful step-by-step reduction
(via repeated `Nat.pred` wrapping) still reaches the same normal form.
**Declaration unblocked:** `Nat.sub_one` (and corrects `Int.pred_toNat`'s
previous reliance on an unsound shortcut).

---

## 2026-07-08: Fix Nat.mod divisor-0 convention (mod n 0 = n, not 0)

### Nat.mod n 0 correctness fix
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** Both the fully-concrete `binary` case and the symbolic partial-iota
case for `Nat.mod` reduced `Nat.mod n 0` to `0`. This is wrong: Lean's `Nat.mod`
returns the dividend, not `0`, when the divisor is `0`. This was a pre-existing
bug (not introduced in this pass) that made `nyaya` treat `n % 0` and `0` as
definitionally equal when they are not (for nonzero `n`), which is a real
unsoundness — an overly permissive rule that could let a false equation type-
check via `rfl`. It was found opportunistically while citing the real `Nat.mod`
definition for the `Fin.castSucc_one` fix above.
**Fix:** Changed both reduction sites: the divisor-0 branch of the concrete
`binary` computation now returns the numerator `m` instead of `Expr.natlit
Z.zero`, and the `is_zero b` partial-iota case now returns `whnf env a` (the
numerator) instead of `Expr.natlit Z.zero`.
**Kernel reference:** `Nat.mod`, `src/Init/Prelude.lean` (leanprover/lean4),
fetched via `cdn.jsdelivr.net/gh/leanprover/lean4@master/...` (a GitHub raw
mirror) after `raw.githubusercontent.com` and the `github.com` blob view were
both rate-limited (429) during this session; verified against the raw fetched
text directly (not a paraphrase). Doc comment on `Nat.mod`, quoted verbatim
from the fetched source:
> The modulo operator, which computes the remainder when dividing one natural
> number by another. Usually accessed via the `%` operator. When the divisor
> is `0`, the result is the dividend rather than an error.
>
> Examples:
>  * `7 % 2 = 1`
>  * `9 % 3 = 0`
>  * `5 % 7 = 5`
>  * `5 % 0 = 5`
>  * `show ∀ (n : Nat), 0 % n = 0 from fun _ => rfl`
>  * `show ∀ (m : Nat), 5 % (m + 6) = 5 from fun _ => rfl`

And the actual definition:
```
protected def Nat.mod : @& Nat → @& Nat → Nat
  | 0, _ => 0
  | n@(succ _), m => ite (LE.le m n) (Nat.modCore n m) n
```
For `m = 0`: `LE.le 0 n` is always true, so this takes the `Nat.modCore n 0`
branch. `Nat.modCore n 0`'s definition (`dite (LT.lt 0 y) (fun hy => ...) (fun
_ => x)` with `y = 0`) takes the `x`-branch since `0 < 0` is false, returning
`x = n` (the dividend) unchanged. So `Nat.mod n 0 = n`, confirming the
docstring's `5 % 0 = 5` example.
**Declaration unblocked:** none directly (no currently-failing `init.export`
declaration in this pass exercised this path); this is a correctness fix for
an already-committed rule, verified not to regress any previously-passing
declaration (see verification below).
**Verification:** fast-tier (`Fin.castSucc_one` still passes) plus full
regression tier — all 12 previously-unblocked declarations (`Fin.zero_le`,
`String.length_singleton`, `Nat.lor.eq_1`, `List.id_run_foldlM`,
`Nat.add_succ_sub_one`, `BitVec.replicate_zero`, `List.get_cons_succ'`,
`ISize.not_xor`, `Vector.findM?_pure`, `BitVec.not_sshiftRight`,
`Int.pred_toNat`, `Float32.toUInt32`) still pass individually.

---

## 2026-07-08: Nat.mod partial iota for concrete positive numerator

### Nat.mod literal-numerator, symbolic-denominator partial iota
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** `Fin.castSucc_one` (proved by `rfl`) failed with a structural mismatch
bottoming out at `Nat.add n##8 0 =?= Nat.add n##8 1`, traced back through
`Fin.val`/`Fin.mk`/`Fin.castSucc` to `Nat.mod 1 (n##8+2) =?= Nat.mod 1 (n##8+3)`
(via `Fin.instOfNat`'s use of `Nat.mod` to compute `OfNat.ofNat (Fin _) 1`). Both
sides needed to reduce to the literal `1`, but `nat_lit_reduce`'s `Nat.mod` case
only handled the fully-concrete case and the two "one side is literal 0" partial
cases — not "numerator is a positive literal, denominator symbolic."
**Fix:** Added a partial iota rule: when the numerator `a` is a concrete positive
`NatLit k` and `Nat.ble b a` (using the existing `Nat.ble` partial-iota chain)
reduces to `Bool.false` (i.e. denominator `b > a`), return `k`. This mirrors the
real `Nat.mod` definition's `ite (LE.le m n) (Nat.modCore n m) n` branch taking
the `n` arm when the `LE.le` condition is false.
**Kernel reference:** `Nat.mod`, `src/Init/Prelude.lean` (leanprover/lean4):
```
protected def Nat.mod : Nat → Nat → Nat
  | 0, _ => 0
  | n@(succ _), m => ite (LE.le m n) (Nat.modCore n m) n
```
Doc comment on `Nat.mod`: "`Nat.mod n (m + n + 1)` should reduce to `n` for
concrete `Nat` literals `n` ... These reductions help `Fin n` literals work
well, because the `OfNat` instance for `Fin` uses `Nat.mod`." Our case
(`Nat.mod 1 (n##8+2)`) is exactly this pattern with literal `n = 1`, `m = n##8`.
**Declaration unblocked:** `Fin.castSucc_one`

---

## 2026-04-13: Nat.mod/Nat.div partial iota for zero first argument

### Nat.mod 0 n and Nat.div 0 n partial iota rules
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** `Fin.zero_le` failed with a structural mismatch:
`OfNat.ofNat Nat 0 (instOfNatNat 0)` vs `Fin.val n (OfNat.ofNat (Fin n) 0 (Fin.instOfNat n _ 0))`.
The RHS reduces through `Fin.instOfNat` → `Fin.ofNat` → `Fin.mk n (HMod.hMod ... 0 n) ...`,
then proj-reduce fires on `Fin.val` to extract the first field, leaving `Nat.mod 0 n`.
The LHS reduces to the Nat literal `0`. The mismatch was `0 =?= Nat.mod 0 n` — the
latter was stuck because `nat_lit_reduce` for `Nat.mod` only had the `binary` case
(both args must be NatLit). With symbolic `n`, no reduction fired.
**Fix:** Add partial iota rules to `Nat.mod` and `Nat.div` for when one arg is zero:
- `Nat.mod 0 n → 0`
- `Nat.mod n 0 → 0` (Lean kernel convention)
- `Nat.div 0 n → 0`
- `Nat.div n 0 → 0` (Lean kernel convention)
**Declaration unblocked:** `Fin.zero_le`

---

## 2026-04-13: String literal projection reduction (2637 declarations checked)

### Proj-reduce string literals by expanding to constructor form
**File:** `tc.ml`, `whnf_impl` Proj case
**Problem:** `String.length_singleton` (proved by `rfl`) requires
`String.length (String.singleton c) =?= 1` definitionally. The reduction chain
expands via `String.push` → `String.casesOn` (struct-eta) → `String.mk (List.append "".data [c])`.
The key step: `String.casesOn` struct-eta generates the projection `"".String.0`
(i.e. `Expr.proj "String" 0 ""`) to extract the `data` field of the empty string
literal `""`. `whnf` reduces this by delegating to the `Proj` case, but the existing
code only handles `Expr.Const`-headed inner terms (constructor applications). A string
literal `Expr.Literal (Expr.StrLit "")` isn't constructor-headed, so proj-reduce
silently gave up and returned `"".String.0` unchanged. This blocked `List.rec` from
firing iota on `List.append [] [c]`, leaving `List.length` stuck at a non-literal.
**Fix:** After `whnf env inner` in the `Expr.Proj` branch, expand any `StrLit s`
to its constructor form via `Reduce.string_lit_to_ctor s` before attempting
proj-reduce. For `""`, this yields `String.mk (List.nil Char)`, whose head
`String.mk` is a constructor, so proj-reduce fires and extracts `List.nil Char`.
The rest of the reduction chain (`List.append [] [c] → [c]`, `List.length [c] → 1`)
then completes normally.
**Declaration unblocked:** `String.length_singleton`

---

## 2026-03-28: Structure eta, lambda eta, Nat symbolic successor iota, bitwise delta

### Narrow Nat builtin delta guard to primitives only
**File:** `tc.ml`, `is_nat_builtin_name`
**Problem:** `Nat.lor.eq_1` failed because `Nat.lor` was in the builtin guard list,
blocking delta-unfolding when `nat_lit_reduce` didn't fire (symbolic args). The
equation lemma needs `Nat.lor` to unfold to `Nat.bitwise Bool.or` to verify
definitional equality.
**Fix:** Remove bitwise operations (`Nat.land`, `Nat.lor`, `Nat.xor`,
`Nat.shiftLeft`, `Nat.shiftRight`, `Nat.testBit`) from `is_nat_builtin_name`. They
still get the O(1) fast path from `nat_lit_reduce` on concrete args, but now fall
through to delta when the fast path doesn't apply. Safe because `Nat.bitwise`
recurses logarithmically (divides by 2), unlike primitive arithmetic (linear).
**Declaration unblocked:** `Nat.lor.eq_1`

### Lambda eta in isDefEq
**File:** `tc.ml`, `isDefEq_impl`
**Problem:** `List.id_run_foldlM` compared `fun x y => Pure.pure Id ... (f x y)`
against `f`. The outermost `Lam` is already WHNF so the `Pure.pure` inside the body
never gets a chance to reduce, and the `Lam` vs `FreeVar` pair has no matching case.
**Fix:** Add lambda eta to the catch-all in `isDefEq_impl`: when one side is a `Lam`
and the other is not, instantiate the lambda body with a fresh variable and apply the
other side to the same variable, then recurse. This lets the `Pure.pure Id` in the
body reach WHNF and reduce away.
**Declaration unblocked:** `List.id_run_foldlM`

### Nat.add/Nat.mul symbolic successor partial iota
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** `Nat.add_succ_sub_one` (`m + succ n - 1 = m + n`) failed because
`Nat.add m (Nat.succ n)` was stuck — the partial iota rules only fired when the
second argument was a concrete `NatLit`, not a symbolic `Nat.succ y`. This blocked
`Nat.sub` from seeing the successor in the result, so the whole expression stayed
unreduced.
**Fix:** Extend partial iota for `Nat.add` and `Nat.mul` to handle symbolic
successor arguments after the `NatLit` check fails:
`Nat.add x (Nat.succ y) → Nat.succ (Nat.add x y)`,
`Nat.mul x (Nat.succ y) → Nat.add (Nat.mul x y) x`.
**Declaration unblocked:** `Nat.add_succ_sub_one`

### Nat.mul partial iota rules
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** `BitVec.replicate_zero` failed with a structural mismatch:
`HMul.hMul Nat Nat Nat (instHMul Nat instMulNat) w (OfNat.ofNat Nat 0 ...)` vs
`OfNat.ofNat Nat 0 ...`. After type class unfolding this is `Nat.mul w 0 =?= 0`,
but `Nat.mul` only had concrete binary reduction (both args literal) and no partial
iota rules for symbolic arguments.
**Fix:** Add partial iota rules for `Nat.mul` (bounded to second arg ≤ 64):
`Nat.mul x 0 → 0`, `Nat.mul x (n+1) → Nat.add (Nat.mul x n) x`.
**Declaration unblocked:** `BitVec.replicate_zero`

### Structure eta in isDefEq
**File:** `tc.ml`, `isDefEq_impl`
**Problem:** `List.get_cons_succ'` still failed after iota structure eta because the
final defeq check compared `Fin.mk n (Nat.add i.0 0) proof` (constructor application)
against `i` (a free variable). The `isDefEq` match had no case for comparing a
constructor-headed term against a non-constructor term and fell through to
"structural mismatch".
**Fix:** Before the catch-all error in `isDefEq_impl`, try structure eta: when one
side is a constructor application of a structure type (single ctor, no indices, not
recursive), compare each constructor field against the corresponding projection of
the other side. For `Fin.mk n val proof` vs `i`, this compares `val =?= i.0`
(succeeds via `Nat.add x 0 → x`) and `proof =?= i.1` (succeeds via proof
irrelevance).
**Declaration unblocked:** `List.get_cons_succ'`

---

## 2026-03-27: Proof-irrelevant App inference & structure eta for iota

### Skip defeq check for Prop-typed arguments in infer App
**File:** `tc.ml`, `infer` App case
**Problem:** Inferring the type of applications whose arguments are proofs (Prop-typed)
triggered expensive defeq checks on auto-generated proof terms (omega, decide, etc.).
These terms can be enormous and normalising them is wasteful since proof irrelevance
means the exact proof doesn't matter.
**Fix:** Before checking `isDefEq btype arg_type`, check whether `btype` is a Prop
(its type whnf's to `Sort 0`). If so, skip the defeq check entirely — any two proofs
of the same Prop are interchangeable.

### Structure eta in iota reduction
**File:** `tc.ml`, `iota_at_head`
**Problem:** `List.get_cons_succ'` (proved by `rfl`) requires
`List.get (a :: as) (Fin.succ i) =?= List.get as i` to hold definitionally. After
reducing through `brecOn`/`List.rec`/`List.get.match_1`, the kernel reaches
`Fin.casesOn` applied to `Fin.succ _ i`. `Fin.succ` unfolds into a match on `i`, but
`i` is a free variable, so the inner `Fin.casesOn` gets stuck. The whole LHS is stuck
at `Fin.rec ... (Fin.rec ... i ...)` and can't match the RHS.
**Fix:** When `iota_at_head` encounters a non-constructor major premise for a
structure-like recursor (single constructor, no indices, not recursive), apply
"structure eta": reduce by projecting out each field from the major premise.
`S.rec params motive minor x → minor (x.0) (x.1) …`
This is the same rule Lean 4's kernel uses for structure recursors.
**Declaration unblocked:** `List.get_cons_succ'`

---

## 2026-03-26: Nat bitwise builtins & early proof irrelevance

### Nat bitwise builtins
**File:** `tc.ml`, `nat_lit_reduce` and `is_nat_builtin_name`
**Problem:** `ISize.not_xor` reduces through ISize → USize → BitVec → Nat, landing on
`Nat.xor` (defined as `Nat.bitwise bne`). Without a kernel builtin, `Nat.xor` was
delta-unfolded to `Nat.bitwise._unary` → `WellFounded.fix` → `Nat.recAux` on 2^64−2.
**Fix:** Added 6 new Nat builtins with O(1) concrete reduction via Zarith:
- `Nat.land` → `Z.logand`
- `Nat.lor` → `Z.logor`
- `Nat.xor` → `Z.logxor`
- `Nat.shiftLeft` → `Z.shift_left`
- `Nat.shiftRight` → `Z.shift_right`
- `Nat.testBit` → `Z.testbit` (returns Bool)

All registered in `is_nat_builtin_name` to block delta-unfolding on symbolic args.
Also fixed latent bug: `Nat.pow` was missing from `is_nat_builtin_name`.

### Early proof irrelevance in isDefEq
**File:** `tc.ml`, `isDefEq_impl`
**Problem:** `isDefEq` whnf'd both sides BEFORE checking proof irrelevance. When
comparing `BitVec.xor._proof_1` proof terms with different arguments, whnf unfolded
`Nat.bitwise_lt_two_pow` → `Nat.rec` on 64 (the bit-width), producing a 512KB trace.
The proof irrelevance check would have short-circuited this, but it ran too late.
**Fix:** Moved proof irrelevance check before whnf. `infer` is memoised so the type
check is cheap, and proofs of the same Prop are immediately equal without reduction.
**Declaration unblocked:** `ISize.not_xor`

---

## 2026-03-26: Projection reduction and Nat partial iota (57+ declarations passing)

### Projection reduction guard
**File:** `tc.ml`, `whnf_impl` Proj case
**Problem:** The Proj case in whnf matched ANY `Expr.Const` head, including stuck
recursors (`List.rec`, `Nat.rec`). This extracted arbitrary recursor arguments as
if they were constructor fields, producing garbage results.
**Fix:** Added `List.mem cname ctor_names` guard to verify the head is actually a
constructor of the projected inductive type before extracting fields.
**Declaration unblocked:** `Vector.findM?_pure`

### Nat.zero normalization
**File:** `tc.ml`, `whnf_impl` Const case
**Problem:** `Nat.zero` (a `Const` node) and `NatLit 0` (a `Literal` node) are
definitionally equal in Lean but are different AST nodes. `isDefEq` had no
Const-vs-Literal case, so `Nat.zero =?= NatLit 0` failed.
**Fix:** whnf normalizes `Const "Nat.zero"` to `NatLit 0`.
**Declaration unblocked:** `BitVec.not_sshiftRight`

### Partial iota for Nat.add
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** `Nat.add x 1` should reduce to `Nat.succ x`, but the `is_nat_builtin`
guard blocked delta-unfolding, and `nat_lit_reduce` only handled fully-concrete args.
**Fix:** Added partial iota rules in `nat_lit_reduce`:
- `Nat.add x 0 → x`
- `Nat.add x (n+1) → Nat.succ (Nat.add x n)` (bounded: n <= 64)

**Declaration unblocked:** `Vector.findM?_pure` (after projection fix)

### Partial iota for Nat.beq
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** `Nat.beq (Nat.succ n) (Nat.succ m)` should reduce to `Nat.beq n m`
but was stuck because the builtin guard blocked delta-unfolding.
**Fix:** Added partial iota rules:
- `Nat.beq (succ n) (succ m) → Nat.beq n m`
- `Nat.beq (succ _) 0 → Bool.false`
- `Nat.beq 0 (succ _) → Bool.false`

**Declaration unblocked:** `BitVec.not_sshiftRight`

### Partial iota for Nat.sub
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** `Nat.sub (Nat.succ m) 1` should reduce to `m`. Same builtin guard issue.
**Fix:** Added partial iota rules (handles both `Nat.succ x` and `NatLit (k+1)`
as successor forms):
- `Nat.sub x 0 → x`
- `Nat.sub 0 _ → 0`
- `Nat.sub (succ n) (succ m) → Nat.sub n m`

**Declaration unblocked:** `Int.pred_toNat`

### Partial iota for Nat.ble
**File:** `tc.ml`, `nat_lit_reduce`
**Problem:** Same pattern as Nat.beq — symbolic args blocked by builtin guard.
**Fix:** Added partial iota rules:
- `Nat.ble 0 _ → Bool.true`
- `Nat.ble (succ _) 0 → Bool.false`
- `Nat.ble (succ n) (succ m) → Nat.ble n m`

**Declaration unblocked:** Proactive (anticipated from BitVec/UInt comparisons)

### Opaque declaration support
**Files:** `tc.ml` (check function), `decl.ml` (get_value)
**Problem:** `Opaque` declarations fell through to the error case in `check`.
**Fix:** Added `Opaque` case to `check` that infers the value's type and checks it
against the declared type (same as `Def`/`Thm`). Opaques remain unfoldable via
`get_value` (matching upstream behavior of exporting opaques as defs).
**Declaration unblocked:** `Float32.toUInt32`

### Nat.pow removed from kernel builtin list
**File:** `tc.ml`, `is_nat_builtin_name`
**Problem:** `Nat.pow` was listed as a kernel builtin, but it's NOT one in the Lean 4
kernel (only `succ`, `add`, `sub`, `mul`, `div`, `mod`, `beq`, `ble` are). This
blocked delta-unfolding of `Nat.pow` on symbolic args, causing `isDefEq` to compare
the stuck `Nat.pow m n` against `Nat.mul n m` and fail.
**Fix:** Removed `Nat.pow` from `is_nat_builtin_name`.
**Note:** Later re-added `Nat.pow` to `is_nat_builtin_name` alongside the bitwise
builtins — it IS needed once the bitwise ops are present.

---

## Earlier work (pre-2026-03-26)

### Core infrastructure
- Expression AST with hash-consing (`lib/expr.ml`)
- Parser for Lean 4 export format (`lib/nyaya_parser/`)
- Declaration types: Axiom, Def, Thm, Opaque, Quot, Inductive, Ctor, Rec (`lib/decl.ml`)
- whnf with delta, beta, iota, zeta reduction (`lib/tc.ml`)
- isDefEq with structural comparison, proof irrelevance (`lib/tc.ml`)
- Type inference (`lib/tc.ml`)
- Nat kernel builtins via `nat_lit_reduce` for O(1) concrete arithmetic
- `is_nat_builtin` guard preventing delta-unfolding of builtins on huge literals
- Level substitution in iota reduction
- Well-posedness checks (duplicate uparams, free variables, type-of-type is Sort)
- Debug trace system for diagnosing failures

---

## Architecture notes

### Nat builtin strategy
The Lean 4 C++ kernel computes Nat builtins natively in O(1). Our OCaml kernel
replicates this with two mechanisms:
1. **`nat_lit_reduce`**: O(1) computation when all args are concrete `NatLit` values
2. **`is_nat_builtin` guard**: Prevents delta-unfolding builtins on symbolic args
   (which would expose O(n) `Nat.brecOn` recursion on huge literals)
3. **Partial iota rules**: For cases where one arg is symbolic but the other is a
   small concrete value or a constructor (`succ`), apply the iota rules directly
   with bounded recursion

### Known remaining work
- Optimization: some declarations run unacceptably slow
- Constructor checks are stubbed out (`check_ctor` always returns true)
- Recursor and Inductive checks are stubbed out
- ~~Missing Nat bitwise builtins~~ (done: `Nat.land`, `Nat.lor`, `Nat.xor`, `Nat.shiftLeft`, `Nat.shiftRight`, `Nat.testBit`)
- Quotient type checking
- Full test suite: 57 of 36688 declarations passing
