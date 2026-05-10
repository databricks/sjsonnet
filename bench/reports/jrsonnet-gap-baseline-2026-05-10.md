# jrsonnet gap baseline

## Scope

This report records the current gap triage source for the stacked performance
exploration branch. The numbers below come from latest fetched
`jrsonnet origin/master:docs/benchmarks.adoc`, not from the older local checked
out `jrsonnet` worktree.

Use this as direction-finding only. Before turning any idea into a PR, re-run
local sjsonnet benchmarks on the stacked branch, compare against current master,
and use source-built jrsonnet for the matching workload.

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

## Immediate optimization priorities

1. **Re-measure foldl/string concat on the stacked branch.** This is the largest
   documented gap, but existing rope/string work may already close much of it.
2. **If foldl remains open, inspect `std.foldl` and string append semantics.**
   Target function-call/lazy-scope allocation first; do not introduce eager
   behavior that changes Jsonnet error timing.
3. **Use large string template as the next string workload guard.** It is the
   next largest unambiguously string-heavy gap after foldl.
4. **Use array comparison, big object, kube-prometheus, realistic2, and
   manifestJsonEx as guard/secondary targets.** These catch overfitted string or
   renderer changes that regress object-heavy real workloads.

## Validation performed for stacked baseline

| Check | Result |
| --- | --- |
| `./mill --no-server -j 1 __.reformat` | Success. |
| `./mill --no-server -j 1 'sjsonnet.jvm[3.3.7]'.test` | Success: 494 passed, 0 failed. |
| Worktree | Clean after validation. |

