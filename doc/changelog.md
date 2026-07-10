# nyaya type checker changelog

Progress log for the type checker implementation (`lib/tc.ml` and related files).
Test target: `init.export` (36688 declarations from Lean 4's Init library).

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
