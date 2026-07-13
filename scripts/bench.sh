#!/usr/bin/env bash
#
# Performance yardstick for the nyaya kernel checker.
#
# Runs the checker with NYAYA_STATS=1 over a corpus of export files and
# tabulates the deterministic work counters (whnf-miss, delta unfolds, defeq
# calls, peak whnf depth) plus CPU time. The counters are reproducible across
# runs and machines, so a diff of two baselines is signal, not noise -- use it
# to measure a reduction change (e.g. lazy-delta) and to catch regressions.
#
# Usage:
#   scripts/bench.sh                 # fast ndjson perf corpus
#   scripts/bench.sh --with-init     # also sweep test/parser/init.export (slow)
#   scripts/bench.sh --timeout 300   # per-file wall-clock cap (default 180s)
#
# Full per-file output is saved under bench/results/<stamp>/, and the summary
# table is echoed and saved alongside as summary.txt.

set -uo pipefail
cd "$(dirname "$0")/.."

# Make Ctrl+C stop the whole run. In a non-interactive bash loop, a child killed
# by SIGINT otherwise just returns control and the loop marches to the next
# target; trapping INT/TERM (plus the per-target rc check below) exits cleanly.
trap 'printf "\n[bench] interrupted -- stopping.\n" >&2; exit 130' INT TERM

BIN=_build/default/bin/main.exe
TIMEOUT=180
WITH_INIT=0

while [ $# -gt 0 ]; do
  case "$1" in
    --with-init) WITH_INIT=1; shift ;;
    --timeout) TIMEOUT="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

echo "building..." >&2
dune build bin/main.exe 2>&1 | head || { echo "build failed" >&2; exit 1; }

STAMP=$(date +%Y%m%d-%H%M%S)
OUTDIR="bench/results/$STAMP"
mkdir -p "$OUTDIR"
SUMMARY="$OUTDIR/summary.txt"

# label|file|mode   (mode: arena = single-file verdict; sweep = check all decls)
targets=(
  "init-prelude|test/good/init-prelude.ndjson|arena"
  "shift-cascade|test/good/perf/shift-cascade.ndjson|arena"
  "app-lam|test/good/perf/app-lam.ndjson|arena"
  "grind-ring-5|test/good/perf/grind-ring-5.ndjson|arena"
)
[ "$WITH_INIT" = 1 ] && targets+=("init.export|test/parser/init.export|sweep")

hdr=$(printf "%-16s %-9s %11s %10s %10s %8s %10s" \
  label verdict whnf-miss delta defeq peak-w cpu-ms)
echo "$hdr" | tee "$SUMMARY"
echo "$(printf '%.0s-' {1..80})" | tee -a "$SUMMARY"

for t in "${targets[@]}"; do
  IFS='|' read -r label file mode <<< "$t"
  if [ ! -f "$file" ]; then
    printf "%-16s %-9s (missing)\n" "$label" "-" | tee -a "$SUMMARY"
    continue
  fi
  log="$OUTDIR/$label.log"
  if [ "$mode" = "arena" ]; then
    env NYAYA_STATS=1 NYAYA_ARENA=1 timeout "$TIMEOUT" "$BIN" "$file" >"$log" 2>&1
    rc=$?
    case "$rc" in
      0) verdict=accept ;; 1) verdict=reject ;; 2) verdict=decline ;;
      124) verdict=TIMEOUT ;; *) verdict="err:$rc" ;;
    esac
  else
    env NYAYA_STATS=1 NYAYA_SWEEP_ALL=1 timeout "$TIMEOUT" "$BIN" "$file" >"$log" 2>&1
    rc=$?
    [ "$rc" = 124 ] && verdict=TIMEOUT || verdict=swept
  fi

  # Stop the whole run if the child was interrupted (SIGINT=130) or killed
  # (SIGTERM=143) -- these only come from signals, never from a checker verdict.
  if [ "$rc" -eq 130 ] || [ "$rc" -eq 143 ]; then
    printf "\n[bench] interrupted -- stopping.\n" >&2
    exit 130
  fi

  # Parse the single [STATS] summary line: "... whnf-miss=N ... delta=N ... peak-whnf-depth=N ... cpu=Nms"
  line=$(grep -m1 '^\[STATS\] .*whnf-miss=' "$log" || true)
  if [ -n "$line" ]; then
    whnf=$(sed -n 's/.*whnf-miss=\([0-9]*\).*/\1/p'        <<<"$line")
    delta=$(sed -n 's/.*delta=\([0-9]*\).*/\1/p'           <<<"$line")
    defeq=$(sed -n 's/.*defeq=\([0-9]*\).*/\1/p'           <<<"$line")
    peak=$(sed -n 's/.*peak-whnf-depth=\([0-9]*\).*/\1/p'  <<<"$line")
    cpu=$(sed -n 's/.*cpu=\([0-9.]*\)ms.*/\1/p'            <<<"$line")
  else
    whnf=- ; delta=- ; defeq=- ; peak=- ; cpu=-
  fi
  printf "%-16s %-9s %11s %10s %10s %8s %10s\n" \
    "$label" "$verdict" "$whnf" "$delta" "$defeq" "$peak" "$cpu" | tee -a "$SUMMARY"
done

echo | tee -a "$SUMMARY"
echo "full per-file output + top-N hotspot tables in: $OUTDIR/" | tee -a "$SUMMARY"
