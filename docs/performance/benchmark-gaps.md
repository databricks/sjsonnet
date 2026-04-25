# Benchmark Gaps vs jrsonnet

This file tracks only the cases where **sjsonnet is slower than jrsonnet**. Do not record wins, ties, or unmeasured guesses.

## Scope

This document is the local source of truth for newly measured gaps. Historical benchmark tables from upstream or jrsonnet docs are useful context, but only add rows here after a local serial run confirms that sjsonnet still trails jrsonnet.

## Gap summary schema

Use one row per measured benchmark case where the gap is still open.

| field | meaning |
| --- | --- |
| `case` | Stable case name, usually the input file path or suite/id pair |
| `suite_or_fixture` | Benchmark suite, resource path, or fixture name |
| `gap_area` | Short hotspot label such as `parser`, `stdlib`, `imports`, `manifest`, `object`, or `unknown` |
| `jrsonnet_mean_ms` | Mean runtime for jrsonnet in milliseconds |
| `sjsonnet_native_mean_ms` | Mean runtime for Scala Native sjsonnet in milliseconds |
| `sjsonnet_jvm_or_graal_mean_ms` | Mean runtime for JVM or GraalVM sjsonnet when measured |
| `sjsonnet_vs_jrsonnet` | `sjsonnet_native_mean_ms / jrsonnet_mean_ms`; only keep rows where this is `> 1.00` |
| `priority` | `P0`, `P1`, or `P2` based on impact and confidence |
| `evidence` | Link to PR comment, issue, local raw output, or report section |
| `status` | `open`, `remeasure`, or `closed` |
| `next_step` | Smallest actionable follow-up to reduce or explain the gap |

## Open gap table

Populate this table only after a serial benchmark run confirms that sjsonnet still trails jrsonnet.

| case | suite_or_fixture | gap_area | jrsonnet_mean_ms | sjsonnet_native_mean_ms | sjsonnet_jvm_or_graal_mean_ms | sjsonnet_vs_jrsonnet | priority | evidence | status | next_step |
| --- | --- | --- | ---: | ---: | ---: | ---: | --- | --- | --- | --- |

## Serial benchmark workflow

Run the comparison on one machine, one benchmark case at a time.

1. Record the environment before timing:

   ```bash
   java -version
   ./mill --version
   uname -a
   ```

2. Prepare both executables before timing. Reuse the same built artifacts for every case in the session.
3. Pin the exact commands in shell variables so the comparison is reproducible:

   ```bash
   SJSONNET_CMD='<path-to-sjsonnet-command>'
   JRSONNET_CMD='<path-to-jrsonnet-command>'
   INPUT='<path-to-benchmark.jsonnet>'
   ```

4. Time the two commands serially with identical output handling:

   ```bash
   hyperfine --warmup 3 --runs 10 \
     "$SJSONNET_CMD $INPUT >/dev/null" \
     "$JRSONNET_CMD $INPUT >/dev/null"
   ```

5. Copy the mean, min, max, and stddev into the PR evidence. Copy the mean runtimes into the gap table and compute `sjsonnet_vs_jrsonnet`.
6. If `sjsonnet_vs_jrsonnet <= 1.00`, do not add a gap row.
7. Re-run only after a code change, toolchain change, or benchmark-environment change; otherwise update `status` instead of duplicating rows.

Do not run multiple Mill, JMH, or hyperfine jobs in parallel. Mill has a file lock and concurrent benchmark jobs distort measurements.

## JMH before/after schema

Use this table for JVM or microbenchmark changes.

| benchmark | param_or_fixture | metric | unit | before_mean | before_error | after_mean | after_error | delta_pct | speedup | winner | notes |
| --- | --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- |

## Scala Native hyperfine before/after schema

Use this table for executable-level comparisons.

| case | command | before_mean | after_mean | min | max | stddev | delta_pct | correctness_check | notes |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | --- |

## jrsonnet comparison schema

Use this table whenever the PR claims to close or narrow a gap against jrsonnet.

| case | method | jrsonnet_mean | sjsonnet_mean | sjsonnet_vs_jrsonnet | target_gap | status |
| --- | --- | ---: | ---: | ---: | ---: | --- |

## PR body template

Paste this into PR descriptions when the PR changes performance-sensitive code. Keep the sections even when a section says `not measured` so PRs stay comparable.

```md
## Motivation

Explain the performance gap or compatibility issue being targeted.

## Key Design Decision

Explain the core implementation decision, why it is semantics-preserving, and why it should help this gap.

## Modification

- Change 1
- Change 2
- Benchmark fixture(s) affected

## Benchmark Results

### Environment

| field | value |
| --- | --- |
| machine |  |
| os |  |
| java/runtime |  |
| sjsonnet command |  |
| jrsonnet command |  |

### JMH before / after

| benchmark | param | unit | before | after | delta_pct | speedup |
| --- | --- | --- | ---: | ---: | ---: | ---: |

### Scala Native hyperfine before / after

| case | before_mean | after_mean | min | max | stddev | delta_pct |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |

### jrsonnet comparison

| case | jrsonnet_mean | sjsonnet_mean | sjsonnet_vs_jrsonnet | status |
| --- | ---: | ---: | ---: | --- |

## Analysis

Interpret wins, regressions, measurement noise, and remaining gaps.

## References

- Upstream commit or PR:
- Benchmark raw output:
- Related tracking row:

## Result

State whether this closes, narrows, or only measures the gap.
```

If no remaining gap is observed, replace the table row with: `No measured case in this PR leaves sjsonnet slower than jrsonnet.`
