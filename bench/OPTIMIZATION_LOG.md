# Optimization Log

## Wave 1: direct self-tailrec detection
- Scope: mark direct self-tail-calls in `StaticOptimizer` without requiring explicit `tailstrict`.
- Outcome: kept.
- Validation:
  - `./mill 'sjsonnet.jvm[3.3.7]'.test.testOnly sjsonnet.TailCallOptimizationTests sjsonnet.EvaluatorTests sjsonnet.AstVisitProfilerTests`
  - `./mill 'sjsonnet.jvm[3.3.7]'.test`
  - `./mill bench.runJmh -i 1 -wi 1 -f 1 'sjsonnet.bench.OptimizerBenchmark.main'`
- Notes:
  - semantic feature is correct and verified
  - optimizer benchmark stayed effectively flat (`~0.552-0.564 ms/op` across short JMH runs)

## Wave 2: AST visit profiler + corpus runner
- Scope: instrument both evaluators, run the success-path corpus, and persist measured counts.
- Outcome: kept.
- Artifacts:
  - `bench/AST_VISIT_COUNTS.md`
  - `sjsonnet/src/sjsonnet/AstVisitProfiler.scala`
  - `sjsonnet/src-jvm-native/sjsonnet/AstVisitCorpusRunner.scala`
- Key corpus findings:
  - hottest old-dispatch arms: `ValidId`, `BinaryOp`, `Val`, `Select`
  - hottest normal tags: `ValidId`, `BinaryOp`, `Val.Literal`, `Select`

## Wave 3: data-driven dispatch/tag reordering
- Scope: reorder old `Evaluator.visitExpr` pattern matches and renumber `Expr.tag` values from the measured corpus.
- Outcome: reverted.
- Reason:
  - benchmark results regressed despite the frequency-guided order
  - `Select`/`Self`/`$`/`Super` share low-tag fast paths in `StaticOptimizer`, which constrains safe tag reshuffling
  - the naive reorder appears to fight Scala/JVM codegen rather than help it
- Rejected measurements:
  - `MainBenchmark.main`: `2.788 ms/op -> 3.222 ms/op`
  - `OptimizerBenchmark.main`: `0.555 ms/op -> 0.569 ms/op`
  - corpus runner new evaluator time: `235 ms -> 261 ms`
- Resolution: restore the original dispatch/tag order and keep the profiler data for later, more targeted optimizations.
