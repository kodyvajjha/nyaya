# Arena perf-track deferrals

Cases from `dune build @runtest` that are **pure performance timeouts** — the
binary is killed at the 30s arena limit (exit 124) with *no* verdict emitted —
rather than a wrong verdict. Per `doc/arena-correctness-loop-plan.md` §"iteration
protocol" step 2, a timeout is a perf problem, not a correctness one; it is the
only permitted exception to strict top-down and is logged here explicitly rather
than silently skipped.

| Case | Group | Observed | Notes |
|------|-------|----------|-------|
| `test/good/perf/app-lam.ndjson` | good (expect accept) | killed at 30s, exit 124, no verdict; repeated `[Env] Inference complete` then hang | 28573-line ndjson; a valid proof nyaya cannot check within the time limit. Not an unsoundness or a crash — deferred to the perf track. |
