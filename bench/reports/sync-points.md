# Performance sync points

This file is the ledger for performance migration and exploration work. Keep it
updated before porting, skipping, or re-testing optimization commits so the same
idea is not repeated without new evidence.

## Active baselines

| Area | Ref | Notes |
| --- | --- | --- |
| Upstream master | `0ae7b78a93c4e643f9bcfb6dd1d99d9fe7a522a9` | Latest `databricks/sjsonnet` master fetched for this branch. |
| Historical JIT branch | `hepinssh/jit@9dc20016b0e2d1a061d1c0451ed555dcc46a0a33` | Source material only; do not mechanically rebase wholesale. |
| Fresh exploration branch | `jit-explore-2026` | Clean branch from upstream master for benchmark-gated, atomic ports. |
| Ready-PR stacked branch | `perf/stacked-ready-gap-explore` | Current gap exploration baseline: upstream master plus ready PRs #825/#826/#828/#833/#834 and the two accepted `jit-explore-2026` micro-optimizations. |
| jrsonnet benchmark source | `jrsonnet origin/master:docs/benchmarks.adoc` | Use fetched `origin/master` contents, not the dirty/stale local jrsonnet checkout. Current gap triage is in `bench/reports/jrsonnet-gap-baseline-2026-05-10.md`. |

## Branch strategy decision

The historical `jit` branch is 104 commits ahead of upstream master and contains
many old experiments that now conflict with newer master-side optimizations. A
mechanical rebase was attempted and stopped at the second commit because it would
force large rewrites of `StaticOptimizer`, `ExprTransform`, `Evaluator`, and
`Val` before proving that the old shape still helps current master.

Decision: keep `hepinssh/jit` as a reference branch, then port or reimplement
only ideas that pass current semantic and benchmark gates.

## Commit migration ledger

| Upstream/source commit | Decision | Reason / next action |
| --- | --- | --- |
| `04b7ff60` `chore: reduce allocation of Num` | Skipped | Superseded by current master `Val.cachedNum` and related no-tuple/static-singleton work. Reapplying the old diff risks undoing newer allocation fixes. |
| `6269dfe2` `refactor static optimizer dispatch` | Deferred | High-conflict historical refactor. Revisit only as a fresh, benchmark-backed `StaticOptimizer` micro-change, not as a full-file port. |
| `068afa11` `add tailrec profiling checkpoint` | Deferred | Diagnostic-only idea. Keep out of production PRs unless converted into a report or benchmark harness improvement. |
| `bfced4ec` `Optimize StaticOptimizer scope map construction` | Ported as current-master variant | Reimplemented the low-risk allocation part in `ScopedExprTransform`: replace `zipWithIndex.map`, tuple creation, and intermediate merge arrays with while-loop `HashMap.updated` construction. Kept `Scope` immutable instead of porting the mutable shell. |
| `f5959f27` `Reuse StaticOptimizer binding scope shell` | Partially skipped | The mutable `Scope.mappings` shell was intentionally not ported after review; it saves only wrapper allocations and weakens the invariant. |
| `f9337010` `Cache parsed dynamic percent formats` | Candidate / verify duplicate | Check current master and open PRs before porting; avoid duplicate `%` format caching. |
| `1e84155c` `Cache sorted object key arrays lazily` | Candidate / verify duplicate | Check overlap with current renderer/materializer key-order work and semantic effects on object visibility. |
| `e98cd1f8` `Optimize format chunk runtime` | Candidate / high scrutiny | Prior #776 format follow-up was not benchmark-positive on current master; only revisit with a materially different micro-benchmark signal. |
| `8b87e03a` `Specialize static object layouts` | Candidate / high risk | Potentially high value but touches object semantics; requires strong regression tests and review. |
| `6367df6d` `Optimize StaticOptimizer apply specialization` | Partially ported | Retained only the stable `tryStaticApply` single-pass `Array[Val]` construction. Tested and rejected the direct `rebindApply` shortcut because repeated JMH was noisy/negative. |
| `926a6d0f` `Optimize sort to use in-place Arrays.sort` | Candidate | Re-test with correctness coverage for Jsonnet ordering and stability expectations. |
| `49a1d51b` `Optimize assertEqual to use structural equality` | Candidate | Requires exact compatibility checks for error messages and deep equality behavior. |
| `ee740182` `Optimize boolean allocation with singleton Val.True/Val.False` | Verify duplicate | Current master already uses static boolean values; likely superseded. |
| `c4ee6be7` `Optimize std.range allocation and cleanup singletons` | Candidate | Compare against current range implementation and Native/JS constraints. |
| `4fa535fb` `Optimize stdlib allocation: foldl while-loop, join pre-sized, flatten two-pass, reverse direct` | Partially superseded | Join/string pieces overlap existing PRs; remaining stdlib loops require per-function benchmark gates. |
| `2d3e56d8` `Optimize foldl string concatenation with StringBuilder` | Verify duplicate | Rope/string-join work may already cover the biggest foldl gap; re-test before porting. |
| `b4b2da5e` `Optimize Materializer: direct array iteration for static objects` | Candidate / high scrutiny | Prior renderer/materializer splits produced regressions; isolate direct iteration only. |
| `d63ce904` `Optimize Renderer: write integer digits directly without String allocation` | Candidate | Re-test against current renderer and direct long-to-chars follow-ups. |
| `1d72a474` `Optimize string rendering: fast path for escape-free strings` | Partially superseded | Current split work already explored escape-safe paths; revisit only with new guard-clean data. |
| Current kube-prometheus profile: `MaterializeJsonRenderer` visitor allocation | Ported as current-stack micro-optimization | Reused stateless `MaterializeJsonRenderer` array/object visitors, following the existing `ByteRenderer` shape. Kube-prometheus Native improved `235.971 +/- 12.925 ms` to `224.975 +/- 11.550 ms` (`-4.66%`) with output equality and no reviewer objections. |
| Current kube-prometheus profile: strict `.json` CRD import parse cost | Ported as current-stack micro-optimization | Added a strict `.json` import fast path that builds literal `Val` trees and falls back to the Jsonnet parser for malformed JSON, duplicate keys, non-finite numbers, recursion-depth overflow, and other semantic edge cases. Kube-prometheus Native improved from `224.975 +/- 11.550 ms` to `139.242 +/- 1.204 ms` (`-38.11%` for this step, `-40.99%` vs original stacked baseline), leaving a `1.58x` source-built jrsonnet gap. |
| `dd90b11a` `Use ASCII bitset for strip chars instead of scala Set` | Existing PR | Covered by PR #789; keep tracking there rather than duplicating. |
| `9dc20016` `Inline arithmetic fast path in tryEagerEval` | Candidate / late-stage | Only after current eager-eval semantics and optimizer tests are audited. |

## Required gate before pushing optimization PRs

1. Search open PRs and current master for duplicate work.
2. Add or update regression tests for any semantic edge being touched.
3. Run formatting and the relevant unit tests locally.
4. Run focused JMH plus nearby guard benchmarks.
5. Run the full test suite before pushing a PR branch.
6. Update this ledger and the relevant PR body with current benchmark evidence.

## Local results

| Change | Evidence |
| --- | --- |
| `ScopedExprTransform` scope-map allocation trim | `OptimizerBenchmark.main`: master `0.432 +/- 0.004 ms/op`, branch `0.422 +/- 0.004 ms/op` (`-2.3%`). |
| Guard benchmark | `MainBenchmark.main`: master `2.223 +/- 0.106 ms/op`, branch `2.204 +/- 0.031 ms/op` (neutral/slightly positive). |
| Tests | `./mill --no-server -j 1 sjsonnet.jvm[3.3.7].test`: 493 passed, 0 failed. `./mill --no-server -j 1 __.test`: success, 2066/2066 tasks. |
| Review | Independent `gpt-5.4` and `claude-sonnet-4.6` code-review agents reported no significant issues. |
| `tryStaticApply` single-pass static value collection | `OptimizerBenchmark.main`: prior branch `0.422 +/- 0.004 ms/op`, candidate `0.414 +/- 0.005 ms/op` on the stable split run. Full historical `rebindApply` shortcut was rejected after repeat runs (`0.414 +/- 0.024`, then `0.430 +/- 0.014`). |
| Guard benchmark | `MainBenchmark.main`: candidate `2.228 +/- 0.062 ms/op`, neutral versus prior branch `2.204 +/- 0.031` and master `2.223 +/- 0.106`. |
| Tests | `./mill --no-server -j 1 sjsonnet.jvm[3.3.7].test`: 493 passed, 0 failed. `./mill --no-server -j 1 __.test`: success, 2066/2066 tasks. |
| Review | Independent `gpt-5.4` and `claude-sonnet-4.6` code-review agents reported no significant issues. |
| Ready-PR stacked gap baseline | Created `perf/stacked-ready-gap-explore` from upstream master, stacked #825/#826/#828/#833/#834 and accepted JIT micro-optimizations. Resolved the #825/#834 Native `CharSWAR` conflict by keeping the four-UTF-16-char ascii-safe SWAR scan and #834 propagation call sites. |
| Stacked baseline validation | `./mill --no-server -j 1 __.reformat`: success. `./mill --no-server -j 1 sjsonnet.jvm[3.3.7].test`: 494 passed, 0 failed. Worktree clean after validation. |
| Latest source-built foldl comparison | Reset and cleaned local `jrsonnet` to `origin/master@5b43fa88`, built `target/release/jrsonnet` with `cargo build --release -p jrsonnet`, built stack branch Scala Native with `sjsonnet.native[3.3.7].nativeLink`, and ran `hyperfine --shell=none --warmup 5 --min-runs 20`. Foldl string concat: sjsonnet `5.293 +/- 0.589 ms`, jrsonnet `8.655 +/- 0.557 ms`. Go `std.foldl`: sjsonnet `4.900 +/- 0.378 ms`, jrsonnet `6.268 +/- 0.505 ms`. Foldl is not a current local gap. |
| Latest source-built large string template comparison | Used `bench/resources/cpp_suite/large_string_template.jsonnet`, verified stdout equality with `cmp`, then ran `hyperfine --shell=none --warmup 5 --min-runs 20`. sjsonnet Scala Native stack: `11.515 +/- 1.085 ms`; source-built jrsonnet: `6.190 +/- 0.975 ms`; current local gap is `1.86x`. |
| Latest source-built big object comparison | Used `bench/resources/cpp_suite/gen_big_object.jsonnet`, verified stdout equality with `cmp`, then ran `hyperfine --shell=none --warmup 5 --min-runs 20`. sjsonnet Scala Native stack: `9.057 +/- 0.666 ms`; source-built jrsonnet: `8.972 +/- 0.819 ms`; result is effectively neutral (`1.01x`). |
| Latest source-built realistic2 comparison | Used `bench/resources/cpp_suite/realistic2.jsonnet`, verified stdout equality with `cmp`, then ran `hyperfine --shell=none --warmup 3 --min-runs 10`. sjsonnet Scala Native stack: `83.932 +/- 1.629 ms`; source-built jrsonnet: `144.282 +/- 2.369 ms`; sjsonnet is faster (`0.58x` jrsonnet time). |
| Latest source-built kube-prometheus comparison | Used `jrsonnet/tests/realworld/entry-kube-prometheus.jsonnet` from `jrsonnet/tests/realworld` with `-J vendor`. `jrb install` failed locally with a reqwest/rustls provider panic, so vendor deps were installed with `/tmp/sjsonnet-tools/jb install`. Verified stdout equality with `cmp` (`7,506,029` bytes), then ran `hyperfine --shell=none --warmup 3 --min-runs 10`. sjsonnet Scala Native stack: `235.971 +/- 12.925 ms`; source-built jrsonnet: `93.188 +/- 6.599 ms`; current local gap is `2.53x`. |
| `MaterializeJsonRenderer` visitor reuse | Reused stateless array/object visitors for `std.manifestJson*`. Validation: `__.reformat`, `sjsonnet.jvm[3.3.7].test` (494/494), kube-prometheus output equality, focused JMH guards, and three independent reviews. Kube-prometheus Native stack improved to `224.975 +/- 11.550 ms` (`-4.66%` vs prior stack); source-built jrsonnet in the same run was `88.576 +/- 0.915 ms`, leaving a `2.54x` gap. |
| `.json` import fast path | Added shared sync/Preloader strict-JSON parse path with duplicate-key, malformed JSON, non-finite number, large-integer, and parser-depth regression coverage. Validation: `__.reformat`, `sjsonnet.jvm[3.3.7].test` (502/502), full `__.test` (2066/2066), Native link, kube-prometheus output equality, focused JMH guards, and three independent reviews. Kube-prometheus Native stack improved to `139.242 +/- 1.204 ms`; source-built jrsonnet in the same run was `88.025 +/- 1.271 ms`, leaving a `1.58x` gap. |
