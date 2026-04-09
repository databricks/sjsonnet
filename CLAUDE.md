# sjsonnet Development Guide


## Build & Test

Uses [Mill](https://mill-build.org/) 1.1.2. Cross-built for Scala 3.3.7, 2.13.18, and 2.12.21 (JVM only).

```bash
# Compile (JVM, Scala 3)
./mill 'sjsonnet.jvm[3.3.7]'.compile

# Run all JVM tests (Scala 3)
./mill 'sjsonnet.jvm[3.3.7]'.test

# Run all tests across all platforms
./mill __.test

# Build assembly JAR
./mill 'sjsonnet.jvm[3.3.7]'.assembly

# GraalVM native image
./mill sjsonnet.graal.nativeImage
```

The assembly JAR is at `out/sjsonnet/jvm/3.3.7/assembly.dest/out.jar`. Run it with `java -Xss100m -jar out.jar`.

## Formatting

We follow the Databricks [Scala style guide](https://raw.githubusercontent.com/databricks/scala-style-guide/refs/heads/master/README.md).

Scala sources are formatted with [scalafmt](https://scalameta.org/scalafmt/) (config in `.scalafmt.conf`). The `SjsonnetCrossModule` and `bench` modules mix in `ScalafmtModule`.

```bash
# Format all sources
./mill __.reformat

# Check formatting without changing files
./mill __.checkFormat
```

CI checks formatting on PRs, so run `reformat` before committing.

## Test Structure

- **Framework**: uTest
- **Test suites** (`sjsonnet/test/resources/`):
  - `test_suite/` — From C++ Jsonnet (upstream). ~183 `.jsonnet` files with `.golden` expected output.
  - `go_test_suite/` — From go-jsonnet.
  - `new_test_suite/` — sjsonnet-specific tests (deep nesting, regex, etc.).
- **Test sources**:
  - `test/src/` — Shared tests (parser, evaluator, renderer, stdlib).
  - `test/src-jvm/` — JVM-only (`MainTests`, file I/O, XxHash64).
  - `test/src-jvm-native/` — JVM + Native (`FileTests`, `ConfigTests`, gzip, md5, sha).
  - `test/src-js/` — Scala.js (`FileTests`).

### Golden file tests

Each `.jsonnet` test file has a `.golden` file containing expected output. Files named `error.*` expect non-zero exit and their `.golden` holds the expected stderr (including stack traces).

**Refresh golden outputs**:
```bash
./sjsonnet/test/resources/refresh_golden_outputs.sh
```

The `.sync_ignore` files in `test_suite/` and `go_test_suite/` list tests to skip when syncing from upstream (e.g. differing YAML parser behavior).

### Writing tests for bug fixes

Every bug fix should include a regression test:

1. Create `new_test_suite/<descriptive_name>.jsonnet` with a minimal reproducer.
2. Create `new_test_suite/<descriptive_name>.jsonnet.golden` with expected output.
   - Success tests: JSON output followed by a newline, using `std.assertEqual` chains ending in `true`.
   - Error tests (filename starts with `error.`): expected stderr including stack traces.
3. Run `./mill 'sjsonnet.jvm[3.3.7]'.test` to verify.

## Debug Stats

The `--debug-stats` flag prints runtime counters and timing to stderr after evaluation:

```bash
sjsonnet --debug-stats myfile.jsonnet
```

Output includes thunk creation counts, function/builtin call counts, comprehension iterations, import/parse counts, and phase timing (eval, materialize). Counters are formatted with stable labels for machine parsing. The `DebugStats` class (`sjsonnet/src/sjsonnet/DebugStats.scala`) is wired through `Interpreter` -> `Evaluator`, with parse counting handled by `CountingParseCache`.

## Benchmarks

JMH benchmarks live in `bench/`. Benchmark suites in `bench/resources/`: `bug_suite/`, `cpp_suite/`, `go_suite/`, `sjsonnet_suite/`.

```bash
# Run all regression tests
./mill bench.runRegressions

# Run specific regressions
./mill bench.runRegressions bench/resources/bug_suite/assertions.jsonnet

# List all benchmark regression files
./mill bench.listRegressions

# Refresh benchmark golden outputs
./bench/resources/refresh_golden_outputs.sh
```

For ad-hoc benchmarking, `hyperfine` is available on the PATH.

## Architecture

### Pipeline

```
Source String → Parser → Expr (AST) → StaticOptimizer → Evaluator → Val → Materializer → ujson.Value → Renderer → Output
```

1. **Parser** (`Parser.scala`): Fastparse-based, produces `Expr` AST.
2. **StaticOptimizer** (`StaticOptimizer.scala`): Constant folding, arity specialization, `ValidId` indexing, static checks.
3. **Evaluator** (`Evaluator.scala`): Walks `Expr`, produces lazy `Val` runtime values. Handles imports, function application, tail-call optimization.
4. **Materializer** (`Materializer.scala`): Walks `Val`, produces `ujson.Value`. Hybrid recursive/iterative to avoid stack overflow.
5. **Interpreter** (`Interpreter.scala`): Orchestrates the full pipeline; main programmatic entry point.

### Key types

- **`Expr`** (`Expr.scala`): AST nodes — literals, binary ops, function defs/calls, objects, arrays, comprehensions, imports. `ExprTags` provides byte tags for fast pattern matching.
- **`Val`** (`Val.scala`): Runtime values — `Str`, `Num`, `Bool` (`True`/`False`), `Null`, `Arr`, `Obj`, `Func`, `Builtin`. `Val.Literal` extends both `Val` and `Expr`.
- **`Eval`** trait: `def value: Val`. Implemented by `Lazy` (deferred, cached) and `Val` (immediate). Arrays and objects hold `Eval`, not `Val`.
- **`ValScope`** (`ValScope.scala`): Array-based lexical scope with copy-on-write extension.
- **`TailCall`**: Sentinel value for tail-call optimization; resolved via trampoline in `TailCall.resolve`.

### Standard library

Implemented as Scala builtins (not Jsonnet code) in `sjsonnet/src/sjsonnet/stdlib/`:
- `StdLibModule` — builds the `std` object combining all modules.
- `ArrayModule`, `StringModule`, `ObjectModule`, `MathModule`, `TypeModule`, `EncodingModule`, `ManifestModule`, `SetModule`, `NativeRegex`.

Platform-specific stdlib extensions:
- JVM: `NativeXz` (xz compression), `NativeGzip`, regex via `re2j`.
- Native: `NativeGzip`, crypto.
- JS: `scala-yaml` for YAML parsing.

### Source directory layout

Shared and platform-specific sources are split across directories:

| Directory | Contents |
|-----------|----------|
| `sjsonnet/src/` | Shared core (parser, evaluator, materializer, stdlib) |
| `sjsonnet/src-jvm/` | JVM main class, `Platform.scala` |
| `sjsonnet/src-jvm-native/` | Shared JVM + Native (`SjsonnetMainBase`, `Config`) |
| `sjsonnet/src-js/` | Scala.js main class, `Platform.scala` |
| `sjsonnet/src-native/` | Scala Native main class, `Platform.scala` |

### Modules

- `sjsonnet.jvm` — JVM library + CLI (`SjsonnetMain`)
- `sjsonnet.js` — Scala.js (CommonJS)
- `sjsonnet.wasm` — WebAssembly (via Scala.js)
- `sjsonnet.native` — Scala Native
- `sjsonnet.graal` — GraalVM native image
- `sjsonnet.jvm.client` / `sjsonnet.jvm.server` — Client-server mode (background daemon)
- `bench` — JMH benchmarks
- `playground` — Browser-based Jsonnet playground

### Error handling

- `Error` — Main exception with message, stack frames (`Error.Frame`), optional cause.
- `ParseError` / `StaticError` — Parse-time and static analysis failures.
- `Error.fail(msg, pos)` throws with position info; `Error.withStackFrame(expr)` adds frames to exceptions.
- Public APIs (e.g. `Interpreter.interpret`) return `Either[String, ujson.Value]`.

## Key Design Decisions

- **Lazy evaluation**: Arrays and objects hold `Eval` (not `Val`). `Lazy` wraps a thunk, caches the result, and clears the closure after first evaluation.
- **Tail-call optimization**: `TailCall` sentinel + trampoline loop in `TailCall.resolve`.
- **Hybrid materialization**: Recursive up to a depth limit, then switches to an explicit stack to avoid `StackOverflowError`.
- **Static optimization**: `StaticOptimizer` does constant folding, resolves variable names to scope indices (`ValidId`), and specializes function application by arity (`Apply0`–`Apply3`, `ApplyBuiltin0`–`ApplyBuiltin4`).
- **Parse caching**: `ParseCache` / `DefaultParseCache` caches parsed ASTs to avoid re-parsing imports.
- **Platform abstraction**: `Path` and `Importer` traits abstract filesystem access across JVM/JS/Native.

## Dependencies

Key runtime dependencies:
- `fastparse` — Parser combinators
- `ujson` — JSON AST and rendering
- `pprint` — Pretty printing
- `scalatags` — HTML generation (for error output)
- `os-lib` — Filesystem (JVM/Native)
- `mainargs` — CLI argument parsing (JVM/Native)
- `snakeyaml` — YAML parsing (JVM)
- `re2j` — Regular expressions (JVM)
- `xz` / `lz4-java` — Compression (JVM)

## CI

GitHub Actions workflows in `.github/workflows/`:
- `pr-build.yaml` — PR validation: compile + test across JVM/JS/WASM/Native × JDK 17/21/25, format check, GraalVM native tests.
- `release-build.yaml` — Release builds (JAR, JS, WASM, playground, native binaries).
