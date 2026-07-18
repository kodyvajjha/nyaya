# nyaya

[![CI](https://github.com/kodyvajjha/nyaya/actions/workflows/ci.yml/badge.svg)](https://github.com/kodyvajjha/nyaya/actions/workflows/ci.yml)

An independent, external type checker for [Lean 4](https://github.com/leanprover/lean4), written in OCaml.

## Status

Work in progress. The checker passes the full `Init` library export (`init.export`, ~36,700 declarations) and a growing corpus of the Lean Kernel Arena's positive/negative test cases under `test/good` and `test/bad`. See `doc/changelog.md` for a detailed history, `doc/soundness-risks.md` for known gaps, and `doc/kernel-derived.md` for which parts of the implementation are transcribed from the reference kernel versus independently re-derived.

## Build & test

```sh
dune build
dune build @runtest
```

## Usage

```sh
dune exec bin/main.exe -- path/to/file.export      # legacy array export format
dune exec bin/main.exe -- path/to/file.ndjson       # newline-delimited JSON export format
```

Set `NYAYA_ARENA=1` to run in single-file verdict mode (used for the Lean Kernel Arena).

## Layout

- `lib/name.ml`, `lib/level.ml`, `lib/expr.ml` — core term representation
- `lib/env.ml`, `lib/decl.ml` — environment and declaration bookkeeping
- `lib/tc.ml` — the type checker: inference, whnf, definitional equality, inductive/recursor validation
- `lib/parser/` — lexer/parser for both export formats
- `test/` — positive (`good/`) and negative (`bad/`) test corpora, plus parser unit tests
- `bench/` — performance harness and historical results
- `doc/` — changelog, soundness-risk tracking, kernel-provenance notes

## Acknowledgements

- The [Lean 4](https://github.com/leanprover/lean4) kernel (`src/kernel/`) itself. Although an effort was made to be puritan and not look at the official kernel implementation, some peeks were irresistable. 
- [ammkrn](https://github.com/ammkrn)'s [nanoda_lib](https://github.com/ammkrn/nanoda_lib), a Rust external kernel for Lean 4, whose design (memoization strategy, `InferFlag`, name caching) informed several of nyaya's own performance and correctness choices.
- ammkrn's excellent [*Type Checking in Lean 4*](https://ammkrn.github.io/type_checking_in_lean4/print.html), the primary reference for the old export format and the design choices.

## AI

Some of the code, and a substantial share of the performance work in particular, was written with [Claude Code](https://claude.com/claude-code) and OpenAI's [codex](https://openai.com/codex/) tool.
