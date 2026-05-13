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
| `large_string_template` | improved by simple-format ASCII-safe propagation; jrsonnet still `~1.34x` faster | `bench/reports/sjsonnet-vs-jrsonnet-gaps.md` |
| kube-prometheus realworld | improved by strict JSON byte import parsing; jrsonnet still `1.55x` faster | `bench/reports/sjsonnet-vs-jrsonnet-gaps.md` |

## Accepted ideas

| idea | status | evidence |
|---|---|---|
| Strict JSON byte import parsing | implemented locally; not committed | `Importer.parseJsonImport` uses `ujson.ByteArrayParser`; `CachedResolvedFile` caches small files as bytes and lazily decodes text; kube Native A/B improved candidate to `132.7/132.1 ms` vs clean `139.4/140.3 ms`. |
| Hybrid sort for inline object materialization | implemented locally; pending PR | `Materializer.computeSortedInlineOrder` keeps insertion sort for ≤16 visible fields and uses in-place quicksort for larger inline objects. Native kube A/B on top of strict JSON bytes improved forward `145.3 -> 140.0 ms` and reverse `151.6 -> 148.9 ms`; output equality and full `__.test` passed. |
| Simple named format ASCII-safe propagation | implemented locally; pending PR | `Format.PartialApplyFmt` returns `Val.Str.asciiSafe` when all static format literals and simple named dynamic values are JSON-string ASCII-safe. Native `large_string_template` improved in both command orders (`8.64 -> 8.01 ms`, `8.65 -> 8.17 ms`); JVM JMH stayed neutral-positive (`0.683 -> 0.677 ms/op`). |

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
| Direct `String.charAt` scan in `visitShortString` | Avoided the reusable `getChars` temp-buffer copy. Output stayed identical and kube Native improved weakly, but `large_string_template` regressed/noised negative in both command orders, so the existing reusable-buffer renderer path was restored. |
| Long strict-JSON imported string values marked ASCII-safe during parse | Mirrored the large Jsonnet string literal optimization for `.json` imports. Output stayed identical, but kube Native reverse A/B favored baseline, so the parse-time scan was removed. |
| Lower parsed Jsonnet string ASCII-safe threshold to `>=128` | Tried to align parser marking with ByteRenderer's long-string cutoff. Output stayed identical, but the parse-time scan regressed kube Native in both command orders. |
| Lazy materialization-time cache for inline-object sorted order | Stored `computeSortedInlineOrder` results back on `Val.Obj` when absent. Output stayed identical, but real kube Native single-run A/B was neutral-to-negative, so the lazy write was removed. |
| Native CLI path-only parse cache | Avoided `ResolvedFile.contentHash()` for the Native CLI to bypass SHA-256/OpenSSL provider work. It linked and preserved output, but Native wall-clock was neutral on `null` and negative/noisy on kube, so the default content-hash cache was restored. |
| Native GC switch to Commix | Attempted to set `nativeGC` to Commix in Mill. Build script compilation failed because the GC API was not exposed on the current Mill build classpath, so the config experiment was reverted. |
| Parser `_asciiSafe` hint for static format safety | Reused the parser's large-string ASCII-safe marker to avoid re-scanning static format literals. Debug stats improved, but Native whole-process `large_string_template` regressed in both command orders, so the hint path was removed. |
| Native manual ASCII-safe string-to-byte copy | Replaced `String.getBytes(0, len, dst, dstPos)` with a manual `charAt` loop for known ASCII-safe strings. Native `large_string_template` regressed heavily in both command orders, so the platform copy stays on `getBytes`. |
| Single-character append in simple format loop | Branched the single-label simple format path to call `StringBuilder.append(Char)` when the dynamic value length is one. Native `large_string_template` regressed in both command orders, so the existing `append(String)` loop remains. |
| ByteRenderer minified object comma path | Specialized direct/generic object rendering to manage comma/empty state locally for minified JSON. Output stayed identical and kube improved weakly, but `large_string_template` regressed/noised negative in both command orders, so the generic renderer path was restored. |
| Native-only long ASCII escaped string renderer | Gated a direct `charAt` long-string renderer to Scala Native to avoid UTF-8 byte-array allocation for escaped ASCII strings. Output stayed identical, but `large_string_template` regressed in both command orders, so the UTF-8 encode plus SWAR scan remains the best path. |
| Inline small-stack cycle tracking | Replaced eager `IdentityHashMap` cycle tracking with four inline identity slots plus overflow map while preserving recursive error behavior. Kube was noise-level and `large_string_template` regressed in both command orders, so eager `IdentityHashMap` tracking was restored. |

## Policy

Before opening a performance PR, rerun focused JMH and Scala Native hyperfine
against the current base and source-built jrsonnet. Keep a change only when the
target benchmark is stable-positive and guard benchmarks do not regress.
