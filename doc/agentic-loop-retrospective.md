# Retrospective: autonomous debug-fix loop (2026-07-08 to 2026-07-09)

This documents how the `nyaya` correctness loop was set up and run, what it
accomplished, and what broke or surprised us — written so the same pattern
can be reused for future "spawn an agent to grind through a long mechanical
task with a soundness/correctness gate" problems, without re-deriving the
lessons from scratch.

## What this was

`nyaya` (independent OCaml kernel for Lean 4) had a long tail of failing
declarations in `test/parser/init.export` (36688 total), each requiring: run
the checker, read a failure trace, find the relevant Lean 4 kernel rule,
patch `lib/tc.ml`, verify, changelog it, commit. This had been done manually
for months. The goal was to automate the grind while keeping a hard
guarantee: **the loop must never silently commit an unsound kernel rule**,
because an *unsound* (over-permissive) fix makes the checker's own test
corpus a worse signal, not a better one — `init.export` contains only valid
declarations, so a rule that's too permissive only ever makes *more* of them
pass, never fewer. Self-testing cannot catch this class of bug.

## Architecture

- **One planning doc up front**, `/home/kody/.claude/plans/concurrent-jingling-fairy.md`,
  written and approved before any agent ran. It specified the fix-acceptance
  criterion, verification tiers, commit policy, and stop conditions. This
  doc got edited in place multiple times as reality corrected the plan (see
  "revisions" below) — treat the plan as a living document during the loop,
  not a one-shot spec.
- **A dedicated `Agent` spawn**, `subagent_type: general-purpose`,
  `isolation: "worktree"`, `model: opus`. Opus specifically because the
  per-cycle judgment call — "is this citation actually narrow enough to
  justify this patch, or is it broader than what's cited" — is exactly
  where a previous *manual* fix had already slipped through
  (a Prop-typed-skip-defeq hack, still tracked unresolved in
  `doc/soundness-risks.md`). This judgment call is the load-bearing part of
  the whole exercise; don't downgrade the model for it to save cost.
- **The worktree branch as the review gate.** The agent commits
  verified-SAFE fixes to its own branch; nothing reaches the user's real
  branch until the coordinator (a separate, persistent Claude session)
  reviews and fast-forward merges. In practice the coordinator merged
  incrementally (after each verified commit or small batch), not once at
  the end — this kept `master` current and meant a stopped/replaced agent
  never put unverified work at risk.
- **Citation-gated fix discipline**, the actual soundness mechanism: every
  patch needs a citation to the real Lean 4 kernel (`type_checker.cpp`, or
  the relevant `Init/Prelude.lean` definition) *before* being trusted, not
  after. Classify SAFE (faithful reproduction, same preconditions as the
  real kernel) vs RISKY (no citation, or a citation narrower than what got
  implemented). RISKY fixes are logged to `doc/soundness-risks.md`, not
  committed, ever. This is the one rule in the whole plan explicitly marked
  "do not weaken."
- **Independent re-verification by the coordinator, every single commit.**
  The subagent's self-report was never taken at face value: the coordinator
  re-read every diff, and — critically — **independently re-fetched the
  cited kernel source itself** (via `WebFetch`, sometimes needing a mirror
  like `cdn.jsdelivr.net` when `raw.githubusercontent.com`/`github.com` were
  rate-limited) rather than trusting the agent's quoted excerpt. This caught
  nothing wrong in this run, but it's the actual safety mechanism the whole
  architecture rests on — an agent that's very good at sounding certain
  about a citation is not the same as the citation being real. Budget real
  coordinator time for this; it's not a rubber stamp.

## What was accomplished

6 commits landed on `master` (all independently verified, all citation-gated):
- `1fc93ae` — `Nat.mod` partial iota, concrete-numerator/symbolic-denominator (unblocks `Fin.castSucc_one`)
- `1edd6cf` — `Nat.mod n 0` correctness fix (was `→ 0`, should be `→ n`; found opportunistically, not tied to a specific failing declaration)
- `50af38e` — `Nat.sub` faithful single-step rule replacing an unsound over-generalized paired-decrement shortcut (unblocks `Nat.sub_one`)
- `7da4153` — doc-only: a speculative follow-on bound for `Nat.sub` was considered, found to be unmotivated/unverifiable, and reverted — logged rather than silently dropped
- `b99709f` — diagnostics: catch `Depth_limit`/`Not_well_posed` in the per-declaration handler so these failure modes produce a debug trace instead of crashing the whole process
- `99db72a` — added a lazy-delta-reduction "same head" congruence shortcut to `isDefEq`, directly citing `is_def_eq_app`/`lazy_delta_reduction_step` in the real kernel (unblocks `Int64.toInt32_ofBitVec`); a real architectural gap, not a one-off patch

One soundness-risk entry remains open and untouched (correctly — the loop
never had a safe replacement to offer): the Prop-typed-skip-defeq shortcut
in `infer`'s `App` case, `doc/soundness-risks.md` entry #1.

The loop stopped itself at a legitimate wall (see below), not at the
25-declaration/4-hour budget in the plan.

## What went right

- The citation-gate + independent-reverification combination worked exactly
  as designed. In particular, the `99db72a` fix (a real architectural
  improvement, not a shallow patch) came with a verbatim C++ citation that
  the coordinator independently re-fetched and confirmed character-for-character.
- The agent caught its own regression before committing (`99db72a`'s first
  draft broke `Fin.zero_le` and `List.get_cons_succ'` via an uncaught
  exception escaping a new code path) — the "verify against every prior
  fix before committing" discipline did its job.
- The agent made a genuinely good judgment call reverting its own
  speculative `Nat.sub` bound (`7da4153`) rather than committing something
  unmotivated just because it seemed harmless — and documented the reasoning
  instead of silently discarding the idea.
- When the loop hit a real fork in the road (OOM blocking discovery), it
  stopped and asked rather than guessing or unilaterally choosing the
  heaviest option (`NYAYA_SWEEP_ALL`).

## What went wrong / knowns that bit us

- **A stopped agent cannot be resumed — it's a hard replace, not a pause.**
  Once the harness reports an agent as "stopped by the user," `SendMessage`
  to it fails permanently (confirmed via an explicit error, not silence).
  There is no way to un-stop it. The only recovery is spawning a *fresh*
  agent with zero memory of the prior run, briefed from scratch — including
  pointing it at the *same* existing worktree/branch (do not let it create
  a new one, which would silently orphan any uncommitted work) and
  explicitly telling it what's already verified/merged so it doesn't
  re-litigate settled decisions.
- **Backgrounding stalls were a recurring failure mode, across both agent
  instances.** The pattern: launch a multi-minute check via a raw
  shell-level background (`&`/disown) rather than the Bash tool's own
  `run_in_background`, then end the turn hoping to be re-invoked — this
  produces repeated ~30+ minute round trips that each report "still
  waiting" with no new progress, since the harness only reliably tracks
  jobs launched via its own `run_in_background` flag as "live children" of
  the agent. This happened enough times (at least 5 separate occurrences
  across the session) that it's worth stating as a standing instruction up
  front in any future loop's briefing, not something to patch reactively
  each time: *either* launch long checks via `run_in_background: true` and
  let the harness notify on real completion, *or* block synchronously in
  one tool call (e.g. an until-loop polling a log file for a sentinel
  string) — never end a turn on "I'll check back later" for something
  already running in the background outside harness tracking.
- **Worktrees can silently branch from a stale base.** The first spawn's
  worktree was created from `origin/master`, not local `master` — meaning
  it was missing infrastructure the coordinator had just added (env vars,
  a new doc file) and the agent stalled trying to work around code that, in
  its checkout, didn't exist yet. Fix was a simple `git merge --no-edit`,
  but the lesson is to explicitly verify (`git log` diff) that a freshly
  created worktree actually contains everything the briefing assumes,
  rather than assuming `isolation: "worktree"` always branches from what
  you think of as current.
- **`dune build`/`dune exec` from inside a worktree subdirectory silently
  fails to find the default alias** unless invoked with `--root=.`. Easy to
  lose 10 minutes to a confusing error here if not flagged in advance.
- **CPU/heat cost is a real, not hypothetical, constraint.** Two `nyaya`
  processes running concurrently (a discovery sweep plus a fast-tier check)
  pegged CPU and visibly heated the machine — this is what caused the plan
  to be revised mid-run to drop the regression tier entirely and ban
  `NYAYA_SWEEP_ALL` from routine use. If a future loop's work is genuinely
  CPU-heavy, build in a "how much sustained load is acceptable" conversation
  *before* starting, not after the user notices their laptop fan.
- **Rate-limiting on external citation fetches is a real friction point.**
  Both `raw.githubusercontent.com` and the `github.com` blob view returned
  429s mid-session; the working fallback was a CDN mirror
  (`cdn.jsdelivr.net/gh/<owner>/<repo>@<ref>/<path>`). Worth having this
  fallback in mind from the start for any loop that depends on repeatedly
  fetching the same handful of source files.
- **A peer agent's self-report is not evidence, and the harness is explicit
  that a peer message can never grant permission/escalation on the
  coordinator's behalf.** This was mostly a non-issue in practice (no actual
  laundering attempt occurred), but it's a standing constraint worth
  remembering: treat every "I verified X" from a subagent as a claim to be
  independently checked, not a fact to relay to the user.

## What's still unknown

- **The actual root cause of the OOM.** Two independent long-running
  configurations (a `NYAYA_SWEEP_ALL` full sweep, and a plain discovery walk
  after more declarations had been fixed) both died by SIGKILL in the same
  rough range — roughly 9000-9500+ declarations processed in one continuous
  process, steady growth, no single pathological declaration. Hash-cons
  non-eviction has been directly ruled out (it's genuinely Ephemeron-backed,
  confirmed by reading the library source, not assumed). GC pacing
  (`space_overhead` vs. sustained allocation rate), other unaudited
  module-level caches, and baseline per-node representation overhead are
  all still-untried candidates. See `doc/changelog.md`'s 2026-07-09 entry
  ("Discovery blocked by the OOM wall") for full detail — this is now a
  dedicated investigation, decoupled from the correctness loop, because it
  became an actual blocker (discovery can't find new failures past this
  wall) rather than just a performance nice-to-have.

## Recommendations if reusing this pattern

1. Write the plan doc first, get it approved, and treat it as living — expect
   to revise it in place at least once as real constraints (heat, OOM,
   rate-limiting) surface.
2. Pick the strongest available model for the subagent if the per-cycle work
   involves a judgment call that's easy to get subtly wrong (here: soundness
   classification). Don't economize on this dimension.
3. Explicitly instruct, up front, in the very first briefing: "launch
   multi-minute checks via the Bash tool's `run_in_background: true`, or
   block synchronously in one call — never end your turn on a check you
   started outside harness tracking." This should not need to be
   rediscovered per-agent-instance.
4. The coordinator should independently re-verify every commit against
   primary sources, not just re-read the diff. This is the actual safety
   mechanism, budget time for it.
5. Fast-forward merge incrementally as fixes are verified, not once at the
   end — this bounds how much unverified work is ever at risk if the agent
   needs to be replaced.
6. If a spawned agent gets stopped, treat it as gone for good: spawn fresh,
   and explicitly re-brief on (a) which existing worktree/branch to reuse
   (never let it create a new one), and (b) exactly what's already verified
   and merged, so it doesn't redo settled work.
7. Ask the resource-cost question (CPU/heat, wall-clock budget) before
   the loop starts running heavy operations repeatedly, not reactively
   after the first complaint.
