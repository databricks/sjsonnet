# sjsonnet vs jrsonnet current gap ledger

This report tracks only locally rechecked gaps where current sjsonnet is still
slower than a source-built jrsonnet reference. The jrsonnet upstream benchmark
document is still useful for broad ranking, but its sjsonnet rows reference older
released builds and must not be treated as current truth without local recheck.

## Baseline

| Field | Value |
|---|---|
| sjsonnet base | `upstream/master` at `cedc083b4676be43e01bdd6f6cb5d7f4432d0d32` |
| sjsonnet binary | Scala Native `sjsonnet.native[3.3.7].nativeLink` |
| jrsonnet reference | `origin/master` at `5e8cbcdbc860a616dbd193428f8933dd7532f537`, `cargo build --release -p jrsonnet` |
| benchmark rule | single benchmark process; no concurrent Mill/JMH/hyperfine |

## Latest confirmed local gaps

| priority | workload | sjsonnet Native | jrsonnet | gap | status | next direction |
|---:|---|---:|---:|---:|---|---|
| 1 | `bench/resources/cpp_suite/large_string_template.jsonnet` | `8.01-8.17 ms` | `6.0 +/- 1.2 ms` | jrsonnet `~1.34x` faster | improved | ASCII-safe propagation for simple named formats closed the largest local slice; remaining gap is mostly startup plus format/render overhead. |
| 2 | `jrsonnet/tests/realworld/entry-kube-prometheus.jsonnet -J vendor` | `132.09 +/- 2.33 ms` | `85.29 +/- 1.12 ms` | jrsonnet `1.55x` faster | improved | Strict JSON byte import parsing reduced sjsonnet Native time by about 5% locally; remaining gap is mostly materialization/rendering and startup. |

## Accepted in this session

| idea | validation | result |
|---|---|---|
| Parse strict `.json` imports from UTF-8 bytes and cache small resolved files as bytes until text is needed | Output equality against jrsonnet and previous sjsonnet on kube-prometheus; focused `PreloaderTests`; Native A/B forward+reverse; focused JMH guards; full `__.test` | kept: kube Native improved from clean `139.4 +/- 2.8 ms` to candidate `132.7 +/- 1.9 ms` forward, and clean `140.3 +/- 2.6 ms` to candidate `132.1 +/- 1.9 ms` reverse. Debug stats parse time improved from prior clean `~88.3ms` to `69.8ms` in the final run. |
| Use in-place quicksort for inline object sorted order when field count is large | `sample` on repeated kube materialization; output equality on kube and `large_string_template`; Native A/B forward+reverse; focused renderer/json tests; `__.checkFormat`; full `__.test` | kept: `sample` reduced `computeSortedInlineOrder` top-stack samples from `164` to `63` and sort-specific samples from `164` to `75`; kube Native improved from `145.3 +/- 3.6 ms` to `140.0 +/- 3.2 ms` forward and from `151.6 +/- 10.2 ms` to `148.9 +/- 3.7 ms` reverse. |
| Preserve ASCII-safe metadata for simple named `%s` format results | Regression tests for safe and unsafe dynamic values; output equality on `large_string_template` and kube-prometheus; JVM test suite; focused JMH guard; Native A/B forward+reverse; jrsonnet reference run | kept: `large_string_template` Native improved in both command orders (`8.64 -> 8.01 ms` forward, `8.65 -> 8.17 ms` reverse). JVM JMH stayed neutral-positive (`0.683 -> 0.677 ms/op`). Kube-prometheus was neutral/noisy, not a target regression. |

## Historical jrsonnet-doc gaps that are no longer primary local gaps

| workload | reason |
|---|---|
| Foldl string concat | Prior stacked recheck showed sjsonnet faster than source-built jrsonnet on extracted foldl workloads. |
| Go `std.foldl` | Prior stacked recheck showed sjsonnet faster than source-built jrsonnet. |
| Big object | Prior stacked recheck was effectively neutral; latest focus stays on larger confirmed gaps. |
| `realistic2` | Prior stacked recheck showed sjsonnet faster than source-built jrsonnet. |
| `large_string_join` | Prior local join work closed the jrsonnet gap; keep as guard only. |

## Rejected in this session

| idea | validation | result |
|---|---|---|
| Raise nested `ByteBuilder` flush threshold from 8 KiB to 64 KiB | Output equality on large-template and kube; kube Native A/B | negative: clean `144.0 +/- 2.1 ms`, candidate `153.3 +/- 15.5 ms`. |
| Raise nested flush threshold to 16 KiB / 32 KiB | Output equality on kube; forward and reverse hyperfine | unstable/noisy: 32 KiB forward looked `~3%` faster, reverse had clean `141.5 +/- 1.6 ms` faster than candidate `143.7 +/- 1.9 ms`. |
| Fast-path single-part parsed string instead of always calling `mkString` | Output equality on `large_string_template`; forward and reverse hyperfine | unstable/noisy: forward candidate `10.4 +/- 0.6 ms` vs clean `10.6 +/- 1.2 ms`; reverse clean `10.3 +/- 0.7 ms` vs candidate `10.5 +/- 0.8 ms`. |
| Add 4 inline object value-cache slots | Output equality on kube; debug stats; forward and reverse hyperfine; focused JMH guards | not enough: overflows `2452 -> 946`, but Native A/B was neutral (`1.00x` in reverse). |
| Add lazy small overflow cache before HashMap | Output equality on kube; debug stats; hyperfine | negative: overflows `2452 -> 83`, but clean `140.9 +/- 1.4 ms` beat candidate `141.7 +/- 2.3 ms`. |
| Mark strict JSON import objects to skip materializer cycle tracking | Output equality on kube; debug stats; forward and reverse hyperfine | not enough: materialize debug time improved, but Native A/B was only weak positive forward and neutral reverse. |
| Parse strict JSON integers through `ParseUtils.parseIntegralNum` before `toDouble` fallback | Output equality on kube and `large_string_template`; JSON fast-path tests; Native kube forward/reverse A/B | not enough: explicit integral scan regressed parse debug time; `decIndex/expIndex` variant removed the scan but remained noisy. Forward median favored candidate, while reverse median/min favored baseline, so it was reverted. |
| Precheck object keys with `Platform.isAsciiJsonSafe` before direct byte copy | Output equality on kube and `large_string_template`; renderer test covering safe, escaped, and Unicode keys; Native kube forward/reverse A/B | negative: forward median/min were weakly positive but mean was worse; reverse favored baseline across mean/median/min (`141.3/140.2/135.8ms` baseline vs `144.2/142.5/138.1ms` candidate). Reverted. |
| Render short strings by scanning `String.charAt` directly instead of copying to the reusable char buffer first | Output equality on kube and `large_string_template`; renderer/json focused tests; Native kube and `large_string_template` forward/reverse A/B | reject: kube moved weakly positive (`140.75ms` baseline to `139.38ms` candidate forward; `157.39ms` baseline to `147.77ms` candidate reverse), but `large_string_template` regressed/noised negative (`10.99ms` baseline to `14.96ms` candidate forward; `10.27ms` baseline to `10.82ms` candidate reverse). The reusable-buffer `getChars` path remains safer for the large-string priority gap. |
| Mark long strict-JSON imported string values as ASCII-safe during parse | Output equality on kube and `large_string_template`; JVM tests; Native kube forward/reverse A/B | reject: debug stats looked lower in one run, but wall-clock did not hold. Forward was noise-level (`141.19ms` baseline vs `138.64ms` candidate mean, nearly identical median/min), while reverse favored baseline (`134.93ms` baseline vs `137.95ms` candidate). Reverted. |
| Lower parsed Jsonnet string ASCII-safe threshold from `>1024` to `>=128` | Output equality on kube and `large_string_template`; JVM tests; Native kube forward/reverse A/B | reject: the extra parse-time scan did not pay back. Forward favored baseline (`142.00ms` vs `147.34ms` candidate mean), and reverse again favored baseline (`139.74ms` vs `142.20ms` candidate mean). Reverted. |
| Lazily cache computed inline-object sorted order during materialization | Output equality on kube and `large_string_template`; JVM tests; Native kube forward/reverse A/B | reject: reduced a repeated-work sampling hotspot in theory, but single-run kube was not stable-positive. Forward only improved median/min while mean worsened (`137.43ms` baseline vs `143.38ms` candidate); reverse favored baseline mean/median (`134.71/133.77ms` baseline vs `135.61/134.03ms` candidate). Reverted. |
| Native CLI path-only parse cache to avoid file content hashing | JVM tests; Native link; output equality on `null`, kube, and `large_string_template`; Native `null` and kube A/B | reject: skipping `contentHash()` was neutral on `null` and negative/noisy on kube. `null` was effectively unchanged (`4.98ms` baseline vs `4.95ms` candidate mean), while kube favored baseline in both command orders (`141.49/137.67ms` baseline vs `141.87/139.48ms` candidate forward; reverse baseline `138.86/136.33ms` vs candidate `168.07/143.16ms`). Reverted. |
| Switch Native release GC from default Immix to Commix | Mill build check | rejected before benchmarking: the current Mill Scala Native plugin API in this build did not expose `GC.commix` through `scalanativelib.api`, and `scala.scalanative.build.GC` was not on the build script classpath. Reverted rather than guessing further. |
| Reuse parser `_asciiSafe` as a static format safety hint | JVM tests; Native link; output equality on `large_string_template`; Native forward/reverse A/B against the accepted simple-format ASCII-safe candidate | reject: debug stats improved (`parse_time` roughly `5.9ms -> 2.5ms` in one run), but whole-process Native wall-clock regressed in both command orders (`8.23ms` baseline vs `9.47ms` candidate forward; `8.00ms` baseline vs `8.47ms` candidate reverse). Reverted rather than trading true wall-clock performance for better internal counters. |
| Native manual ASCII-safe string-to-byte copy | Native link; output equality on `large_string_template`; Native forward/reverse A/B against the accepted simple-format ASCII-safe candidate | reject: replacing Native `String.getBytes(0, len, dst, dstPos)` with a manual `charAt` loop was much slower in both command orders (`7.89ms` baseline vs `10.78ms` candidate forward; `7.97ms` baseline vs `11.17ms` candidate reverse). Reverted. |
| Append single-character simple format values with `StringBuilder.append(Char)` | JVM tests; Native link; output equality on `large_string_template`; Native forward/reverse A/B against the accepted simple-format ASCII-safe candidate | reject: the single-character branch regressed Native in both command orders (`7.97ms` baseline vs `8.65ms` candidate forward; `7.97ms` baseline vs `8.84ms` candidate reverse). Reverted. |
| Specialize ByteRenderer minified object comma/empty-state handling | JVM compile; Native link; output equality on kube and `large_string_template`; kube and `large_string_template` Native forward/reverse A/B | reject: broad direct-object specialization improved kube weakly (`130.32ms -> 128.31ms` forward; `130.62ms -> 129.74ms` reverse), but `large_string_template` regressed/noised negative (`10.24ms -> 10.45ms` forward; `10.99ms -> 11.27ms` reverse). The sorted-inline-only variant was also unstable, so the generic `flushBuffer`/emptyBits path remains safer. |
| Native-only long ASCII escaped string renderer | Native link; output equality on kube, `large_string_template`, and a long Unicode fallback guard; `large_string_template` Native forward/reverse A/B | reject: avoiding `str.getBytes(UTF_8)` with a Native-only two-pass `charAt` renderer regressed the largest guard in both command orders (`9.96ms -> 11.77ms` forward; `9.56ms -> 10.54ms` reverse). Reverted before kube A/B. |
| Inline small-stack cycle tracking before `IdentityHashMap` overflow | JVM tests; Native link; output equality on kube and `large_string_template`; shallow/deep recursive error equality; kube and `large_string_template` Native forward/reverse A/B | reject: preserving cycle semantics with four inline slots did not pay back. Kube was noise-level, while `large_string_template` regressed in both command orders (`11.59ms -> 12.32ms` forward; `9.67ms -> 10.80ms` reverse). Reverted. |

## Current hypothesis

Large-template remains ratio-priority after the simple-format ASCII-safe win, but
the gap is now much smaller and includes whole-process startup. Kube-prometheus
improved through byte-based strict JSON imports, but source-built jrsonnet is
still about `1.55x` faster. Next work should profile the remaining
materialization/render/startup costs and target a larger structural cost rather
than a single parameter or small cache tweak.

## 2026-05-13 native-vs-jvm split

Re-profile shows the `large_string_template` gap is largely a Scala Native
runtime artifact, not an algorithmic gap in the formatter or parser:

| metric | value |
|---|---|
| JVM JMH (warm) `RegressionBenchmark.main` | `0.873 ms/op` |
| Native cold hyperfine | `~14.5 ms` mean, `9.8 ms` min |
| Native `--debug-stats` (single run, with timing overhead) | parse `12.8ms` + eval `14.6ms` + materialize `2.3ms` |
| jrsonnet native | `5.5 +/- 0.5 ms` |
| sjsonnet Native trivial-startup (`null`) | `~6.6 ms` mean, `5.5 ms` min |
| jrsonnet trivial-startup (`null`) | `~3.9 ms` mean, `2.7 ms` min |

Implication: ~5.5ms of the sjsonnet 14.5ms is process startup. Actual work
ratio min-to-min is roughly `4.3ms / 2.1ms` = ~2x, not 3.4x. Stream-render to
stdout is already in place (`SjsonnetMainBase.renderNormal` uses `ByteRenderer`
directly to `stdoutStream` in `case None if stdoutStream != null`), so the
final output stage is already byte-streamed.

The remaining double work is: `Format.format` builds a full ~590KB String into
a `StringBuilder`, then `BaseByteRenderer.visitString` re-scans that String for
JSON escape chars. Removing this double scan requires routing the renderer into
`Format` so format chunks are escaped and emitted as they are produced. That is
a structural cross-cutting change touching the Format ABI and several stdlib
callers; it is not a single-file micro-optimization and warrants explicit user
go-ahead before implementation.

## 2026-05-13 rejected: visitLongString chunked-char copy

Rewrote `BaseByteRenderer.visitLongString` to avoid the `str.getBytes(UTF-8)`
allocation by scanning chars directly, copying ASCII runs via
`Platform.copyAsciiStringRangeToBytes` (which wraps `String.getBytes(srcBegin,
srcEnd, dst, dstPos)`), and emitting escapes inline.

JVM JMH guard on `large_string_template`:

| variant | iter median (ms/op) |
|---|---:|
| clean baseline (5 runs) | `0.82` (range `0.79-0.92`) |
| chunked-char path (5 runs) | `1.21` (range `1.20-1.24`) |

Result: **+46% JVM regression**. JVM's intrinsified `String.getBytes(UTF-8)` on
the whole string plus a single SWAR scan is faster than per-chunk
`String.getBytes(srcBegin, srcEnd, dst, dstPos)` calls. The hypothesized Native
gain (skip a ~600KB allocation per long string) was not measured, but the
shared-code JVM cost makes the change unshippable per PR-rule-#19 (no
regression). Rejected without Native A/B; pursuing platform-gating would add
complexity disproportionate to the unproven benefit.

## 2026-05-13 rejected: lazy simple-named format byte rendering

Explored a structural version of "Format renders directly to bytes" for large
`%(key)s` object format strings. The implementation kept `Format.format`
unchanged for string semantics, added a lazy `Val.Str` representation only for
large simple-named formats, forced object key lookups up front to preserve error
timing, and taught `ByteRenderer` to render pre-escaped format pieces directly.

Three variants were tried:

| variant | JVM JMH `large_string_template` | Native forward A/B | Native reverse A/B | decision |
|---|---:|---:|---:|---|
| per-static-chunk escaped byte arrays | `0.81-0.85 ms/op` | baseline `10.05ms`, candidate `10.40ms` | baseline `10.53ms`, candidate `10.94ms` | reject |
| flat static byte buffer + offsets | `0.73-0.74 ms/op` | baseline `10.23ms`, candidate `10.39ms` | baseline `10.40ms`, candidate `10.61ms` | reject |
| flat static bytes + pre-escaped dynamic bytes | `0.73-0.78 ms/op` | baseline `10.070ms`, candidate `10.047ms` | baseline `9.890ms`, candidate `10.226ms` | reject |

Conclusion: this direction improves warm JVM JMH but does not improve the
Scala Native whole-process target. The extra Native work to pre-escape and
retain byte slices offsets the avoided final `StringBuilder`/renderer scan, and
the only positive Native run was within noise and reversed when command order
changed. Code was reverted; no runtime optimization retained.
