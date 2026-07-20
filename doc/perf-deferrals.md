# Arena perf-track deferrals

Cases from `dune build @runtest` that are **pure performance timeouts** — the
binary is killed at the 30s arena limit (exit 124) with *no* verdict emitted —
rather than a wrong verdict. Per `doc/arena-correctness-loop-plan.md` §"iteration
protocol" step 2, a timeout is a perf problem, not a correctness one; it is the
only permitted exception to strict top-down and is logged here explicitly rather
than silently skipped.

| Case | Group | Observed | Notes |
|------|-------|----------|-------|
| ~~`test/good/perf/app-lam.ndjson`~~ | ~~good (expect accept)~~ | **FIXED 2026-07-20** | Was: killed at 30s, no verdict. Root cause was eager, one-binder-at-a-time substitution in `infer`'s `Lam`/`Forall` cases; fixed via environment-threaded inference (see `doc/changelog.md`'s 2026-07-20 entry). Now accepts in ~50ms. Kept here, struck through, as a pointer to that entry rather than deleted outright. |
| `test/parser/init.export` (full sweep, `NYAYA_ARENA=1`) | good (expect accept) | killed manually at 2m39s, RSS already 9GB and still climbing | Module-scale memory, not time. For comparison, the arena's `official` checker peaks at 9.1GB checking *all* of Mathlib (~50x Init's size) -- nyaya's Init-alone footprint already matches that. Most likely driven by the never-reset `whnf_closed_memo`/`num_loose_bvars` caches added in `64d89ae`/`def49b9`: a good trade for a single small file, but memory now scales with total corpus size rather than per-declaration working set. Module-scale checkers (`init.yaml`/`std.yaml`/`mathlib.yaml`/`cslib.yaml` in the arena) are `skip-on-ci: true` so this doesn't block the arena PR, but nyaya isn't memory-viable at that scale yet -- needs bounded/evictable caching before it's worth revisiting.
