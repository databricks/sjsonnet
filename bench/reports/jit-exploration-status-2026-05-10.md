# JIT exploration status

## Summary

The historical `hepinssh/jit` branch should be treated as a performance idea
archive, not as a branch to rebase wholesale. It contains useful benchmark notes
and many plausible micro-optimizations, but the first rebase pass already showed
that old full-file rewrites conflict with current master optimizations. The new
working strategy is to start from upstream master and port only isolated,
benchmark-positive ideas.

## Evidence from the aborted mechanical rebase

| Observation | Evidence |
| --- | --- |
| Historical branch size | `git log --reverse upstream/master..hepinssh/jit` lists 104 commits. |
| First commit skipped | `04b7ff60` conflicted with current master numeric caching and singleton work. |
| Second commit blocked | `6269dfe2` conflicted across `Evaluator.scala`, `ExprTransform.scala`, `StaticOptimizer.scala`, and `Val.scala`. |
| Main risk | Large historical rewrites can silently remove newer master-side performance and compatibility fixes. |

## Initial optimization backlog

These items are not approved PRs yet. Each one needs duplicate checks, semantic
tests, focused benchmarks, and full-suite validation before it can become a PR.

| Priority | Candidate | Why it remains interesting | Main risk |
| ---: | --- | --- | --- |
| 1 | Current-master `StaticOptimizer` scope allocation trim | Historical branch had multiple wins around scope construction and binding rewrites. | Static scope bugs are semantic bugs; needs strong parser/evaluator tests. |
| 2 | Apply specialization cleanup | The AST already has arity-specialized apply nodes; removing residual generic overhead may help without runtime semantics changes. | Duplicating existing specialization or weakening static checks. |
| 3 | Parser number/string no-work fast paths | External gaps and old reports point to string/parser allocation pressure. | Jsonnet lexical edge cases around underscores, escapes, and Unicode. |
| 4 | Dynamic `%` format cache audit | Old JIT branch records a large dynamic-format win. | May already be implemented; caching must not capture mutable evaluation state. |
| 5 | Static object layout specialization | Object-heavy workloads remain a major gap vs jrsonnet. | Object visibility, `super`, asserts, hidden fields, and ordering semantics. |
| 6 | Sorted visible-key cache audit | Manifest/object rendering repeatedly sorts keys. | Hidden-field visibility and object mutation/lazy field interactions. |
| 7 | `std.sort` primitive numeric fast path | Historical branch includes primitive double sorting. | Jsonnet ordering rules, NaN behavior, and mixed-type arrays. |
| 8 | `std.range` lazy/direct representation | Range-heavy comprehensions can avoid allocating strict arrays. | Array identity, indexing, materialization, and Native/JS behavior. |
| 9 | `std.map`/`filterMap` non-capturing scope reuse | Historical branch repeatedly targets scope reuse in stdlib loops. | Closure/capture detection must be exact. |
| 10 | Comprehension scope reuse | High call/loop surfaces; historical commits target body and scope construction. | Captured locals and lazy semantics. |
| 11 | Eager evaluation for provably simple function args | Can reduce thunk allocation for pure literals/valid-id binary ops. | Jsonnet laziness and error timing must stay compatible. |
| 12 | Eager object field evaluation for self-free fields | Potential win for object-heavy configs. | `self`, `super`, asserts, hidden fields, and error timing. |
| 13 | Materializer direct array iteration | Avoids iterator/boxing costs in hot manifest paths. | Must preserve recursion-depth fallback and stack-safety. |
| 14 | Inline-object materializer iteration | Historical branch has direct inline object iteration commits. | Field order and visibility semantics. |
| 15 | Renderer integer/long direct write | Avoids transient `String` allocation in number-heavy output. | Exact JSON rendering compatibility and negative/min-value handling. |
| 16 | Renderer bulk append helpers | May reduce per-character overhead for common delimiters and indentation. | Prior renderer splits showed guard regressions; benchmark guards are mandatory. |
| 17 | Strip ASCII lookup representation | Already represented by PR #789; continue there instead of duplicating. | The current PR has weak JVM signal; Native gain is small. |
| 18 | Structural `assertEqual` audit | Could reduce expensive rendered comparisons. | Error output and equality semantics must match existing expectations. |
| 19 | Object single-field storage | Avoiding `LinkedHashMap` in tiny objects may help realistic configs. | Object update, field lookup, and materialization invariants. |
| 20 | Arithmetic fast path in optimizer eager eval | Latest historical JIT commit points at compile-time arithmetic folding. | Must not change overflow, error, or type-error behavior. |

## Execution policy

- Work from `jit-explore-2026`, not the historical `jit` branch.
- Keep commits atomic and source-traceable.
- Prefer reimplementation on current master over applying old patches.
- Reject any idea that changes Jsonnet semantics, makes benchmark guards worse,
  or duplicates an open PR without improving it.
- Record skipped ideas in `bench/reports/sync-points.md` immediately.
