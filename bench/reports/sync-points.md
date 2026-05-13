# Performance sync points

This file tracks current performance migration and exploration work so the same
idea is not repeated without new evidence.

## Active baselines

| Area | Ref | Notes |
|---|---|---|
| upstream/master | `cedc083b4676be43e01bdd6f6cb5d7f4432d0d32` | Clean base used for current local rechecks. |
| jrsonnet | `5e8cbcdbc860a616dbd193428f8933dd7532f537` | Source-built with `cargo build --release -p jrsonnet`. |

## Current confirmed gaps

| workload | status | report |
|---|---|---|
| `large_string_template` | open; jrsonnet `3.49x` faster | `bench/reports/sjsonnet-vs-jrsonnet-gaps.md` |
| kube-prometheus realworld | improved by strict JSON byte import parsing; jrsonnet still `1.55x` faster | `bench/reports/sjsonnet-vs-jrsonnet-gaps.md` |

## Accepted ideas

| idea | status | evidence |
|---|---|---|
| Strict JSON byte import parsing | implemented locally; not committed | `Importer.parseJsonImport` uses `ujson.ByteArrayParser`; `CachedResolvedFile` caches small files as bytes and lazily decodes text; kube Native A/B improved candidate to `132.7/132.1 ms` vs clean `139.4/140.3 ms`. |
| Hybrid sort for inline object materialization | implemented locally; pending PR | `Materializer.computeSortedInlineOrder` keeps insertion sort for ≤16 visible fields and uses in-place quicksort for larger inline objects. Native kube A/B on top of strict JSON bytes improved forward `145.3 -> 140.0 ms` and reverse `151.6 -> 148.9 ms`; output equality and full `__.test` passed. |

## Rejected ideas

| idea | reason |
|---|---|
| Nested byte-buffer flush threshold 16/32/64 KiB | Not stable positive under same-run forward/reverse Native A/B. |
| Single-part parsed string fast path | Not stable positive under same-run forward/reverse Native A/B. |
| 4-slot object value cache | Reduced overflow count but produced only neutral Native wall-clock results. |
| Lazy small overflow cache before HashMap | Reduced overflow count further but regressed Native wall-clock. |
| Strict JSON object cycle-check skip marker | Debug stats improved, but same-run Native A/B was not stable enough to keep. |
| visitLongString char/range-copy path | Stable JVM JMH regression on `large_string_template` (`~0.82ms` baseline to `~1.21ms` candidate); rejected before Native A/B. |
| Lazy simple-named format byte rendering | Three structural variants improved/held JVM JMH but were neutral-to-negative on Scala Native whole-process `large_string_template`; code reverted. |
| Strict JSON integer parse via `ParseUtils.parseIntegralNum` | Tried both an explicit integral scan and the parser-provided `decIndex/expIndex` fast path. Output stayed identical, but kube Native A/B was not stable-positive; reverse median/min favored the existing `toString.toDouble` path. |
| ByteRenderer ASCII-safe object key precheck | Replaced direct key rendering with `Platform.isAsciiJsonSafe` + low-byte copy for safe keys. Output stayed identical, but kube Native reverse A/B favored the existing short-string renderer across mean/median/min. |

## Policy

Before opening a performance PR, rerun focused JMH and Scala Native hyperfine
against the current base and source-built jrsonnet. Keep a change only when the
target benchmark is stable-positive and guard benchmarks do not regress.
