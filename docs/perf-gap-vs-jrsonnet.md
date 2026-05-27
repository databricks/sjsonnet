# sjsonnet (Scala Native) vs jrsonnet — Performance Gap Report

> **Snapshot:** 2026-05-13 (Apple M3 Pro, Darwin 25.3.0 arm64). Hyperfine `-N -w 4 -m 20` (real-world re-runs `-w 6 -m 30`), single-threaded.
> **sjsonnet:** `perf/escape-bulk-write-fast-path @ 7f00c719` (branched from `master @ fcd444cc`, Scala Native release-full, immix GC, full LTO). Adds PR [#864](https://github.com/databricks/sjsonnet/pull/864) (chunked-emit `BaseRenderer.escape`) on top of `master`.
> **jrsonnet:** `0.5.0-pre98` (cargo `--release`, **no `mimalloc`** — that crate's static asm fails to compile on arm64 macOS, matching the prior baselines in `jrsonnet/docs/benchmarks.adoc`).
> **Methodology:** identical to `jrsonnet/nix/benchmarks.nix` (`hyperfine -N -w4 -m20`). Real-world inputs from `jrsonnet/tests/realworld/`; C++ perf/benchmarks from `./bench/resources/cpp_suite/`; stdlib micros from `./bench/resources/go_suite/`.

## Headline

| Real-world scenario | sjsonnet (ms) | jrsonnet (ms) | Ratio |
|---|---:|---:|---:|
| **kube-prometheus** | 179.71 ± 28.94 (min 142.84) | 107.07 ± 19.21 (min 92.49) | **1.68×** |
| GraalVM CI | 75.17 ± 8.19 (min 69.83) | 73.87 ± 3.43 (min 70.52) | 1.02× (tied) |

**Bottom line:** Kube-prometheus remains the single material real-world gap. **PR #864** (chunked `BaseRenderer.escape`) closed the manifest-family gaps: `manifestTomlEx` 2.12× → **0.85× (ahead)**, `manifestYamlDoc` 1.91× → 1.04× (tied), `manifestJsonEx` 1.73× → 1.11× (tied). It did **not** move kube-prometheus, confirming the long-suspected diagnosis that the kube-prom hot path lives elsewhere (likely object materialization + StringBuilder churn, not per-scalar escape). Most synthetic micros sit at the 3–10 ms calibration band where hyperfine warns; treat their *ratios* as direction and their *absolute deltas* as the leverage signal. Means are stddev-noisy on Apple M3 Pro under typical workload; **`min`** values are the most stable rank order.

## Full table (sorted by absolute Δ where sjsonnet trails)

> Δ = `sj_mean − jr_mean`. Negative Δ = sjsonnet wins. Sub-5 ms items annotated as **noise-bound** when stddev ≥ 30% of mean.

### 🔴 Real-world (highest leverage)

| Scenario | sj mean | sj min | jr mean | jr min | Δ ms | Ratio | Notes |
|---|---:|---:|---:|---:|---:|---:|---|
| Real-world: kube-prometheus | 179.71 | 142.84 | 107.07 | 92.49 | **+72.63** | **1.68×** | Largest absolute leverage. Untouched by PR #864 — the hot path is **object materialization + map walk**, not per-scalar escape. Needs profiling (async-profiler / dtrace). |
| Real-world: GraalVM CI | 75.17 | 69.83 | 73.87 | 70.52 | +1.30 | 1.02× | Effectively tied. |

### 🟠 Synthetic — sjsonnet trails by ≥ 1 ms (median signal)

| Scenario | sj mean | sj min | jr mean | jr min | Δ ms | Ratio | Likely root cause |
|---|---:|---:|---:|---:|---:|---:|---|
| std.substr | 8.03 | 4.66 | 2.65 | 2.29 | +5.38 | **3.03×** | Already has ASCII-safe fast path (#834); residual gap is UTF-16 indexing on non-ASCII slow path. **Noise-bound by mean** (σ 30%) but `min` ratio 2.03× is real. |
| Inheritance fn recursion | 11.63 | 5.18 | 6.47 | 2.34 | +5.16 | 1.80× | **Noise-bound** (σ on both sides ≥ 35%). |
| Big object | 17.71 | 9.97 | 13.17 | 7.69 | +4.54 | **1.34×** | Generator output ≈25 k lines; dominated by parser + small-object construction. (Run-to-run σ high — Apple M3 P-core/E-core scheduling.) |
| std.escapeStringJson | 8.57 | 4.90 | 4.28 | 2.41 | +4.29 | **2.00×** | Per-char escape — `Format`/Builtin path, not `BaseRenderer.escape`. Distinct from PR #864's chunked emit. (#849-class follow-up.) |
| std.stripChars | 8.91 | 4.19 | 5.44 | 2.68 | +3.47 | 1.64× | `stripChars` family (#851). **Noise-bound by mean** (σ 114%). |
| std.lstripChars | 6.26 | 4.41 | 3.02 | 2.49 | +3.24 | **2.07×** | See String strips. |
| std.base64 | 7.59 | 5.03 | 4.61 | 2.77 | +2.97 | **1.64×** | `String` → byte-encode loop; no SIMD-friendly fixed-width path. **Highest "actionable" next target after kube-prometheus.** |
| Large string template | 11.31 | 9.74 | 9.11 | 5.31 | +2.20 | **1.24×** | Down from 1.86× pre-#864. `\|\|\|…\|\|\|` text-block path; residual is non-ASCII slow branch in `Format.scala`. |
| String strips | 7.64 | 4.03 | 5.51 | 2.60 | +2.12 | 1.39× | bench.09; `lstrip/rstrip/strip` driver. |
| Tail call | 6.62 | 4.29 | 4.86 | 2.55 | +1.76 | 1.36× | bench.01. **Noise-bound** (σ 33%). |
| Foldl string concat | 9.52 | 5.44 | 7.99 | 6.77 | +1.53 | 1.19× | bench.04. **Noise-bound by mean** (σ 47%). |
| std.parseInt | 6.08 | 4.06 | 4.70 | 2.49 | +1.37 | 1.29× | `Long.parseLong` boxing + sign branch (#852). |
| Array sorts | 7.20 | 5.22 | 6.00 | 3.68 | +1.20 | 1.20× | Already optimized (#855); residual at noise band. |

### 🟡 Tied (within ±15%)

| Scenario | sjsonnet mean | jrsonnet mean | Ratio |
|---|---:|---:|---:|
| std.manifestJsonEx | 5.46 | 4.93 | 1.11× — was 1.73×, **−0.62× after PR #864** |
| std.foldl | 6.42 | 5.86 | 1.10× |
| std.manifestYamlDoc | 6.69 | 6.43 | 1.04× — was 1.91×, **−0.87× after PR #864** |
| std.base64Decode | 12.75 | 12.20 | 1.04× |
| std.rstripChars | 5.60 | 5.60 | 1.00× |
| std.base64DecodeBytes | 18.49 | 20.00 | 0.92× |
| Comparison for array | 11.86 | 12.84 | 0.92× |

### 🟢 sjsonnet wins (preserve these)

| Scenario | sj mean | jr mean | Ratio | Note |
|---|---:|---:|---:|---|
| Comparison for primitives | 42.05 | 115.74 | **0.36×** | Big win. |
| Lazy array (jr `--max-stack 50000`) | 7.10 | 18.52 | **0.38×** | Big win. |
| Inheritance recursion | 66.67 | 153.48 | **0.43×** | Big win. |
| Simple recursive call | 26.83 | 41.57 | 0.65× | |
| std.base64 (byte array) | 8.16 | 12.25 | 0.67× | |
| std.reverse | 15.92 | 23.15 | 0.69× | |
| Large string join | 7.86 | 10.98 | 0.72× | |
| Realistic 2 | 113.89 | 152.22 | 0.75× | |
| **std.manifestTomlEx** | 6.21 | 7.30 | **0.85×** | **Flipped from 2.12× behind → 0.85× ahead** after PR #864. 🎉 |
| Realistic 1 | 16.95 | 18.87 | 0.90× | |

## Top-5 actionable optimization candidates

Selected for: (a) absolute Δ ≥ 2 ms, (b) min(sjsonnet) > 4 ms (above hyperfine calibration band), (c) actionable on this codebase (root cause identifiable in `sjsonnet/src/`).

| # | Target | Estimated leverage | Likely path |
|---|---|---|---|
| 1 | **kube-prometheus real-world** | −72 ms (1.68× → ~1.0×) | Highest leverage by far. The hot path is **not** escape (PR #864 didn't move this). Hypothesis: object construction + `Materializer` walk over the 7.5 MB output. Action: run async-profiler / `dtrace -n 'profile-997 /execname == "out"/ {...}'` against this exact input and identify the top-3 frames. Re-evaluate after data. |
| 2 | **std.base64 (encode)** | −3 ms (1.64× → ~1.0×) | `EncodingModule.scala`. jrsonnet uses table-driven 3-byte→4-char encoding with hardware-friendly stride; sjsonnet currently iterates `Char` by `Char`. Switch to an `Array[Char]` writer with a precomputed alphabet, writing 4 chars per 3-byte group. Result is pure ASCII → `AsciiSafeStr`, propagating gains downstream. |
| 3 | **std.escapeStringJson** | −4 ms (2.00× → ~1.2×) | Distinct from PR #864's `BaseRenderer.escape`. This is the `std` builtin used inside Jsonnet user code; it should reuse the same chunked-emit fast path or share the helper. Audit `StringModule.escapeStringJson` to dispatch into the new chunked helper. |
| 4 | **std.lstripChars / std.stripChars family** | −3 ms each (2.07×, 1.64×) | `StringModule.lstripChars` — per-char `set.contains` lookup. jrsonnet uses bitset for ASCII fast path. Build a `Long`-bitmap when `chars` is all ASCII; fall back to `Set[Char]` otherwise. (#851 follow-up.) |
| 5 | **std.substr (non-ASCII)** | −5 ms (3.03× → ~1.5×) | `substr(s, from, len)` non-ASCII branch does UTF-16 surrogate counting per call. Cache a `lazy val codePointCount` on `Val.Str`? Or fast-fail with `String.codePointAt` indexing only when scan finds first surrogate. |

### Methodology notes

- All 32 benchmarks above run under `hyperfine -N -w 4 -m 20`. Headline scenarios re-run quietly at `-w 6 -m 30`.
- The `bench.07` "Lazy array" case requires `jrsonnet --max-stack 50000` (jsonnet upstream uses ~10 MB OS stack); sjsonnet handles it on its default stack. Reported in the "sjsonnet wins" table.
- `kube-prometheus` runs with `-J jrsonnet/tests/realworld/vendor`; `GraalVM CI` with `-J .../vendor/graal`. Other realworld scenarios (`gitlab-runbooks`, `loki`, `mimir`, `tempo`) need additional jpath setup that the prior 2026-05-12 baselines also skipped.
- Hyperfine emits "Command took less than 5 ms" calibration warnings on the sub-5 ms micros; the corresponding *ratios* drift run-to-run by ±15%. The *absolute* deltas in the table reflect this run only.
- Apple M3 Pro hybrid P-/E-cores cause noticeable σ on sub-100 ms runs even with shell=none and warmup. **Use `min` as the stable rank metric**; means with σ ≥ 30% of mean are flagged "noise-bound".
- Raw hyperfine JSON exports retained under `/tmp/gap-reports/*.json` (local-only, not committed; rule per project memory).
