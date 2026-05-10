# jrsonnet gap baseline

## Scope

This report records the current gap triage source for the stacked performance
exploration branch. The numbers below come from latest fetched
`jrsonnet origin/master:docs/benchmarks.adoc`, not from the older local checked
out `jrsonnet` worktree.

The `jrsonnet/docs` rows are stale for at least foldl/string concatenation.
Before turning any idea into a PR, re-run local sjsonnet benchmarks on the
stacked branch, compare against current master, and use source-built jrsonnet for
the matching workload.

## Stacked exploration baseline

Branch: `perf/stacked-ready-gap-explore`

Built from `upstream/master`, then stacked:

| Source | Commit in stack | Included work |
| --- | --- | --- |
| #825 `perf/constant-array-join` | `da5b0450` | Constant/repeated string join fast paths. |
| #826 `perf/native-lazy-array-stack` | `190c2cc6` | Identity function composition optimization. |
| #828 `perf/toml-render-fastpath` | `0ee2b953` | TOML manifestation fast path. |
| #833 `split/pr776-single-spec-format` | `e21eb029` | Single-spec format builder skip. |
| #834 `split/pr776-ascii-substr` | `15d225e3` | ASCII-safe literal propagation for length/substr. |
| `jit-explore-2026` docs | `6185c820` | Fresh JIT/performance tracking docs. |
| `jit-explore-2026` scope trim | `bde01b9a` | Current-master immutable scope-map allocation trim. |
| `jit-explore-2026` static apply | `b867a317` | Single-pass static apply argument collection. |

The #834/#825 Native `CharSWAR` conflict was resolved by keeping the
four-UTF-16-char `isAsciiJsonSafe` scan from #825 and the ascii-safe propagation
call sites from #834. The stacked branch has no duplicate `isAsciiJsonSafe`
definitions.

## Latest documented gaps vs jrsonnet

These rows are copied from latest `jrsonnet origin/master:docs/benchmarks.adoc`
and should be treated as a stale ranking until replaced by local source-built
hyperfine data.

| Rank | Workload | Scala target | Scala ms | Rust ms | Gap | Initial direction |
| ---: | --- | --- | ---: | ---: | ---: | --- |
| 1 | C++ benchmarks / foldl string concat | GraalVM | 420.9 | 3.7 | 113.76x | Re-test after rope/string work; investigate foldl string-builder/rope semantics only if still open. |
| 2 | C++ benchmarks / foldl string concat | Native | 328.6 | 3.7 | 88.81x | Same as above; Native string concat allocation pressure likely dominates. |
| 3 | Go builtins / `std.foldl` | Native | 80.0 | 2.2 | 36.36x | Audit stdlib fold loops and function-call/lazy scope overhead. |
| 4 | Go builtins / `std.foldl` | GraalVM | 55.4 | 2.2 | 25.18x | Same direction; likely JVM call/closure overhead. |
| 5 | C++ perf_tests / large string template | Native | 14.5 | 2.1 | 6.90x | String template/rendering allocation and concat chain analysis. |
| 6 | C++ perf_tests / large string template | GraalVM | 13.9 | 2.1 | 6.62x | Same direction; check overlap with #825/#834 first. |
| 7 | Go builtins / array comparison | GraalVM | 64.3 | 10.0 | 6.43x | Deep equality/comparison hot path; semantic risk around lazy values/errors. |
| 8 | Real world / kube-prometheus manifests | Native | 204.6 | 43.6 | 4.69x | End-to-end object/materializer/rendering profile after stacked branch. |
| 9 | C++ benchmarks / big object | Native | 3756.8 | 839.2 | 4.48x | Object layout, visible-key cache, materializer object traversal. |
| 10 | Real world / kube-prometheus manifests | GraalVM | 183.7 | 43.6 | 4.21x | Same as Native, but separate JVM allocation profile. |
| 11 | Go builtins / `std.base64` | Native | 4.8 | 1.2 | 4.00x | Check if current base64 SIMD/byte-array PR work already supersedes this doc. |
| 12 | Go builtins / array comparison | Native | 39.1 | 10.0 | 3.91x | Same as GraalVM array comparison. |
| 13 | C++ perf_tests / large string join | Native | 9.7 | 2.6 | 3.73x | Covered by #825; re-measure stacked branch before more join work. |
| 14 | C++ benchmarks / big object | GraalVM | 3035.7 | 839.2 | 3.62x | Same object/materializer direction. |
| 15 | Go builtins / `std.base64Decode` | Native | 4.2 | 1.2 | 3.50x | Check current base64 work and Native byte-array behavior. |
| 16 | C++ perf_tests / realistic2 | Native | 417.8 | 120.9 | 3.46x | Guard benchmark for renderer/materializer/object changes. |
| 17 | Go builtins / `std.manifestTomlEx` | Native | 2558.8 | 750.5 | 3.41x | Covered by #828; re-measure stacked branch before more TOML work. |
| 18 | C++ perf_tests / realistic2 | GraalVM | 409.1 | 120.9 | 3.38x | Same as Native; required guard for string/rendering changes. |
| 19 | Go builtins / `std.manifestJsonEx` | Native | 2443.6 | 748.5 | 3.26x | Renderer/materializer direct iteration; previous broad #776 split regressed guards. |
| 20 | Go builtins / `std.substr` | Native | 2991.7 | 917.8 | 3.26x | Covered by #834; re-measure stacked branch before more substr work. |

## Local source-built hyperfine results

The local `jrsonnet` checkout was reset and cleaned, then rebuilt from latest
`origin/master`:

| Project | Ref / build | Binary |
| --- | --- | --- |
| sjsonnet | `perf/stacked-ready-gap-explore@2e5ef3ea`, `./mill --no-server -j 1 'sjsonnet.native[3.3.7]'.nativeLink` | `out/sjsonnet/native/3.3.7/nativeLink.dest/out` |
| jrsonnet | `origin/master@5b43fa88`, `cargo build --release -p jrsonnet` | `target/release/jrsonnet` |

Method:

- Foldl inputs were copied directly from latest
  `jrsonnet/docs/benchmarks.adoc`; remaining workloads used the repository
  benchmark files listed in the table.
- Correctness smoke compared stdout with `cmp` before measuring.
- Small/medium workloads used `hyperfine --shell=none --warmup 5 --min-runs
  20`; larger real/object workloads used the exact commands listed below.
- `jrsonnet --features mimalloc` was attempted but does not compile on the
  current aarch64 macOS toolchain because `mimalloc-sys` emits x86_64 `%gs`
  inline assembly for this target. The valid local jrsonnet comparison is the
  default release profile above.

| Workload | sjsonnet Scala Native stack | jrsonnet source release | Ratio |
| --- | ---: | ---: | ---: |
| C++ benchmarks / foldl string concat | `5.293 +/- 0.589 ms` | `8.655 +/- 0.557 ms` | sjsonnet is `0.61x` jrsonnet time (`1.64x` faster). |
| Go builtins / `std.foldl` | `4.900 +/- 0.378 ms` | `6.268 +/- 0.505 ms` | sjsonnet is `0.78x` jrsonnet time (`1.28x` faster). |
| C++ perf_tests / large string template | `11.515 +/- 1.085 ms` | `6.190 +/- 0.975 ms` | sjsonnet is `1.86x` jrsonnet time. |
| C++ benchmarks / big object (`bench/resources/cpp_suite/gen_big_object.jsonnet`) | `9.057 +/- 0.666 ms` | `8.972 +/- 0.819 ms` | sjsonnet is `1.01x` jrsonnet time; effectively neutral. |
| C++ perf_tests / realistic2 (`bench/resources/cpp_suite/realistic2.jsonnet`) | `83.932 +/- 1.629 ms` | `144.282 +/- 2.369 ms` | sjsonnet is `0.58x` jrsonnet time (`1.72x` faster). |
| Real world / kube-prometheus manifests (`jrsonnet/tests/realworld/entry-kube-prometheus.jsonnet`, `-J vendor`) | `235.971 +/- 12.925 ms` | `93.188 +/- 6.599 ms` | sjsonnet is `2.53x` jrsonnet time. |

Result: foldl/string concatenation is no longer a current gap on this local
source-built comparison. Large string template remains a current gap, but the
measured gap is `1.86x`, not the stale documented `6.90x`. Big object is no
longer a meaningful local gap and realistic2 is faster in sjsonnet. The largest
confirmed local source-built gap in this recheck set is kube-prometheus at
`2.53x`.

## Immediate optimization priorities

1. **Prioritize kube-prometheus for end-to-end object/materializer/rendering
   work.** Local source-built hyperfine shows a current `2.53x` real-world gap
   after stacking ready PRs.
2. **Keep large string template as the next string-heavy target.** Local
   source-built hyperfine shows a remaining `1.86x` gap. Use
   `large_string_join`, `realistic2`, and `manifestJsonEx` as guards.
3. **Move past foldl/string concat.** Local source-built hyperfine shows the
   stacked sjsonnet Scala Native binary faster than latest source-built
   jrsonnet on both extracted foldl workloads.
4. **Use big object and realistic2 as guards, not primary targets for now.** Big
   object is neutral and realistic2 is already faster than source-built
   jrsonnet in the current stack.
5. **Re-measure array comparison and manifestJsonEx as guard/secondary
   targets.** These catch overfitted string or renderer changes that regress
   object-heavy real workloads.

## Validation performed for stacked baseline

| Check | Result |
| --- | --- |
| `./mill --no-server -j 1 __.reformat` | Success. |
| `./mill --no-server -j 1 'sjsonnet.jvm[3.3.7]'.test` | Success: 494 passed, 0 failed. |
| `./mill --no-server -j 1 'sjsonnet.native[3.3.7]'.nativeLink` | Success; produced a 17M native binary. |
| `cargo build --release -p jrsonnet` | Success; produced `jrsonnet 0.5.0-pre98`. |
| foldl hyperfine | sjsonnet Scala Native stack is faster than source-built jrsonnet on both foldl workloads. |
| large string template hyperfine | Outputs matched; sjsonnet Scala Native stack `11.515 +/- 1.085 ms`, source-built jrsonnet `6.190 +/- 0.975 ms`, so sjsonnet is `1.86x` slower. |
| big object hyperfine | Outputs matched; sjsonnet Scala Native stack `9.057 +/- 0.666 ms`, source-built jrsonnet `8.972 +/- 0.819 ms`, so the result is effectively neutral. |
| realistic2 hyperfine | Outputs matched; sjsonnet Scala Native stack `83.932 +/- 1.629 ms`, source-built jrsonnet `144.282 +/- 2.369 ms`, so sjsonnet is faster. |
| kube-prometheus hyperfine | Installed real-world vendor deps with `jsonnet-bundler` after `jrb install` failed locally with a reqwest/rustls provider panic. Outputs matched (`7,506,029` bytes); sjsonnet Scala Native stack `235.971 +/- 12.925 ms`, source-built jrsonnet `93.188 +/- 6.599 ms`, so sjsonnet is `2.53x` slower. |
| Worktree | Clean after validation. |

## Kube-prometheus optimization checkpoint

First accepted kube-prometheus experiment: reuse the stateless
`MaterializeJsonRenderer` array/object visitors used by `std.manifestJson*`.
The real-world profile showed kube-prometheus spends visible time in
`std.manifestJsonEx` while building Grafana dashboard ConfigMap strings, and
the existing `ByteRenderer` already uses the same visitor-reuse shape for the
fused output path.

| Check | Result |
| --- | --- |
| JVM tests | `./mill --no-server -j 1 'sjsonnet.jvm[3.3.7]'.test`: 494 passed, 0 failed. |
| Native output smoke | `cmp` matched source-built jrsonnet output for `entry-kube-prometheus.jsonnet` (`-J vendor`). |
| kube-prometheus Native hyperfine | Before: `235.971 +/- 12.925 ms`; after: `224.975 +/- 11.550 ms`; delta `-4.66%`. Source-built jrsonnet in the same candidate run: `88.576 +/- 0.915 ms`, so the remaining gap is `2.54x`. |
| Focused JMH guards | `bench/resources/go_suite/manifestJsonEx.jsonnet`: `0.052 ms/op`; `bench/resources/cpp_suite/realistic2.jsonnet`: `40.068 ms/op`; `bench/resources/cpp_suite/large_string_template.jsonnet`: `1.137 ms/op`. |
| Review | Independent `gpt-5.4`, `claude-opus-4.7`, and `claude-sonnet-4.6` code-review agents reported no significant issues. |
