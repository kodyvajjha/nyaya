# nyaya type checker changelog

Progress log for the type checker implementation (`lib/tc.ml` and related files).
Test target: `init.export` (36688 declarations from Lean 4's Init library).

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
