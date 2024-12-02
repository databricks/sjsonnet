# Sjsonnet

A JVM implementation of the [Jsonnet](https://jsonnet.org/) configuration
language.

## Usage

Sjsonnet can be used from Java:

```xml
<dependency>
    <groupId>com.databricks</groupId>
    <artifactId>sjsonnet_2.13</artifactId>
    <version>0.4.12.1</version>
</dependency>
```

```java
sjsonnet.SjsonnetMain.main0(
    new String[]{"foo.jsonnet"},
    new DefaultParseCache,
    System.in,
    System.out,
    System.err,
    os.package$.MODULE$.pwd(),
    scala.None$.empty()
);
```

From Scala:

```scala
"com.databricks" %% "sjsonnet" % "0.4.12.1" // SBT
ivy"com.databricks::sjsonnet:0.4.12.1" // Mill
```

```scala
sjsonnet.SjsonnetMain.main0(
    Array("foo.jsonnet"),
    new DefaultParseCache,
    System.in,
    System.out,
    System.err,
    os.pwd, // working directory
    None
);
```

As a standalone executable assembly:

- <https://github.com/databricks/sjsonnet/releases/download/0.4.12.1/sjsonnet.jar>

```bash
$ curl -L https://github.com/databricks/sjsonnet/releases/download/0.4.12.1/sjsonnet-0.4.12.1.jar > sjsonnet.jar

$ chmod +x sjsonnet.jar

$ ./sjsonnet.jar
error: Need to pass in a jsonnet file to evaluate
usage: sjsonnet [sjsonnet-options] script-file

  -i, --interactive  Run Mill in interactive mode, suitable for opening REPLs and taking user input
  -n, --indent       How much to indent your output JSON
  -J, --jpath        Specify an additional library search dir (right-most wins)
  -o, --output-file  Write to the output file rather than stdout
  ...

$ ./sjsonnet.jar foo.jsonnet
```

Or from Javascript:

```javascript
$ curl -L https://github.com/databricks/sjsonnet/releases/download/0.4.12/sjsonnet-0.4.12.1.js > sjsonnet.js

$ node

> require("./sjsonnet.js")

> SjsonnetMain.interpret("local f = function(x) x * x; f(11)", {}, {}, "", (wd, imported) => null)
121

> SjsonnetMain.interpret(
    "local f = import 'foo'; f + 'bar'", // code
    {}, // extVars
    {}, // tlaVars
    "", // initial working directory

    // import callback: receives a base directory and the imported path string,
    // returns a tuple of the resolved file path and file contents or file contents resolve method
    (wd, imported) => [wd + "/" + imported, "local bar = 123; bar + bar"],
    // loader callback: receives the tuple from the import callback and returns the file contents
    ([path, content]) => content
    )
'246bar'
```

Note that since Javascript does not necessarily have access to the
filesystem, you have to provide an explicit import callback that you can
use to resolve imports yourself (whether through Node's `fs` module, or
by emulating a filesystem in-memory)

### Running deeply recursive Jsonnet programs

The depth of recursion is limited by JVM stack size. You can run Sjsonnet with increased
stack size as follows:

```bash
java -Xss100m -cp sjsonnet.jar sjsonnet.SjsonnetMain foo.jsonnet
```

The -Xss option above is responsible for JVM stack size. Please try this if you
ever run into `sjsonnet.Error: Internal Error ... Caused by: java.lang.StackOverflowError ...`.

There is no analog of `--max-stack`/`-s` option of [google/jsonnet](https://github.com/google/jsonnet).
The only stack size limit is the one of the JVM.

## Architecture

Sjsonnet is implementated as an optimizing interpreter. There are roughly 4
phases:

- `sjsonnet.Parser`: parses an input `String` into a `sjsonnet.Expr`, which is a
  [Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) representing
  the Jsonnet document syntax, using the
  [Fastparse](https://github.com/lihaoyi/fastparse) parsing library

- `sjsonnet.StaticOptimizer` is a single AST transform that performs static
  checking, essential rewriting (e.g. assigning indices in the symbol table for
  variables) and optimizations. The result is another `sjsonnet.Expr` per input
  file that can be stored in the parse cache and reused.

- `sjsonnet.Evaluator`: recurses over the `sjsonnet.Expr` produced by the
  optimizer and converts it into a `sjsonnet.Val`, a data structure representing
  the Jsonnet runtime values (basically lazy JSON which can contain function values).

- `sjsonnet.Materializer`: recurses over the `sjsonnet.Val` and converts it into
  an output `ujson.Expr`: a non-lazy JSON structure without any remaining
  un-evaluated function values. This can be serialized to a string formatted in a
  variety of ways

These three phases are encapsulated in the `sjsonnet.Interpreter` object.

Some notes on the values used in parts of the pipeline:

- `sjsonnet.Expr`: this represents `{...}` object literal nodes, `a + b` binary
  operation nodes, `function(a) {...}` definitions and `f(a)` invocations, etc..
  Also keeps track of source-offset information so failures can be correlated
  with line numbers.

- `sjsonnet.Val`: essentially the JSON structure (objects, arrays, primitives)
  but with two modifications. The first is that functions like
  `function(a){...}` can still be present in the structure: in Jsonnet you can
  pass around functions as values and call then later on. The second is that
  object values & array entries are _lazy_: e.g. `[error 123, 456][1]` does not
  raise an error because the first (erroneous) entry of the array is un-used and
  thus not evaluated.

- Classes representing literals extend `sjsonnet.Val.Literal` which in turn extends
  _both_, `Expr` and `Val`. This allows the evaluator to skip over them instead of
  having to convert them from one representation to the other.

## Performance

Due to pervasive caching, sjsonnet is much faster than google/jsonnet. See
this blog post for more details:

- [Writing a Faster Jsonnet Compiler](https://databricks.com/blog/2018/10/12/writing-a-faster-jsonnet-compiler.html)

Here's the latest set of benchmarks I've run (as  of 18 May 2023) comparing Sjsonnet against
google/go-jsonnet and google/jsonnet, measuring the time taken to
evaluate an arbitrary config file in the Databricks codebase:

|              | Sjsonnet 0.4.3 | google/go-jsonnet 0.20.0 | google/jsonnet 0.20.0 |
| :----------- | -------------: | -------------: | -------------: |
| staging/runbot-app.jsonnet (~6.6mb output JSON) | ~0.10s |  ~6.5s | ~67s |

Sjsonnet was run as a long-lived daemon to keep the JVM warm,
while go-jsonnet and google/jsonnet were run as subprocesses, following typical
usage patterns. The Sjsonnet command
line which is run by all of these is defined in
`MainBenchmark.mainArgs`. You need to change it to point to a suitable input
before running a benchmark or the profiler.

Benchmark example:

```
sbt bench/jmh:run -jvmArgs "-XX:+UseStringDeduplication" sjsonnet.MainBenchmark
```

Profiler:

```
sbt bench/run
```

There's also a benchmark for memory usage:

Execute and print stats:
```
sbt 'set fork in run := true' 'set javaOptions in run ++= Seq("-Xmx6G", "-XX:+UseG1GC")' 'bench/runMain sjsonnet.MemoryBenchmark'
```

Execute and pause - this is useful if you want to attach a profiler after the run and deep dive the
object utilization.
```
sbt 'set fork in run := true' 'set javaOptions in run ++= Seq("-Xmx6G", "-XX:+UseG1GC")' 'bench/runMain sjsonnet.MemoryBenchmark --pause'
```

## Laziness

The Jsonnet language is _lazy_: expressions don't get evaluated unless
their value is needed, and thus even erroneous expressions do not cause
a failure if un-used. This is represented in the Sjsonnet codebase by
`sjsonnet.Lazy`: a wrapper type that encapsulates an arbitrary
computation that returns a `sjsonnet.Val`.

`sjsonnet.Lazy` is used in several places, representing where
laziness is present in the language:

- Inside `sjsonnet.Scope`, representing local variable name bindings

- Inside `sjsonnet.Val.Arr`, representing the contents of array cells

- Inside `sjsonnet.Val.Obj`, representing the contents of object values

`Val` extends `Lazy` so that an already computed value can be treated as
lazy without having to wrap it.

Unlike [google/jsonnet](https://github.com/google/jsonnet), Sjsonnet caches the
results of lazy computations the first time they are evaluated, avoiding
wasteful re-computation when a value is used more than once.

## Standard Library

Different from [google/jsonnet](https://github.com/google/jsonnet), Sjsonnet
does not implement the Jsonnet standard library `std` in Jsonnet code. Rather,
those functions are implemented as intrinsics directly in the host language (in
`Std.scala`). This allows both better error messages when the input types are
wrong, as well as better performance for the more computationally-intense
builtin functions.

## Client-Server

Sjsonnet comes with a built in thin-client and background server, to help
mitigate the unfortunate JVM warmup overhead that adds ~1s to every invocation
down to 0.2-0.3s. For the simple non-client-server executable, you can use

```bash
./mill -i show sjsonnet[2.13.4].jvm.assembly
```

To create the executable. For the client-server executable, you can use

```bash
./mill -i show sjsonnet[2.13.4].server.assembly
```

By default, the Sjsonnet background server lives in `~/.sjsonnet`, and lasts 5
minutes before shutting itself when inactive.

Since the Sjsonnet client still has 0.2-0.3s of overhead, if using Sjsonnet
heavily it is still better to include it in your JVM classpath and invoke it
programmatically via `new Interpreter(...).interpret(...)`.

## Publishing

To publish, make sure the version number in `build.sc` is correct, then run the following commands:

```bash
./mill -i mill.scalalib.PublishModule/publishAll --sonatypeCreds lihaoyi:$SONATYPE_PASSWORD --publishArtifacts __.publishArtifacts --release true

./mill -i show sjsonnet[2.13.4].js.fullOpt
./mill -i show sjsonnet[2.13.4].jvm.assembly
```

Please ensure that you are publishing with JDK 8, e.g. via 
`JAVA_HOME=/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home/`,
to ensure the output bytecode remains compatible with users on older JVMs.

## Changelog

### 0.4.12.1
- Fix a bug leading to the truncation of the data returned by `std.gzip` and `std.xz` [#221](https://github.com/databricks/sjsonnet/pull/221).

### 0.4.12
- Fix a bug introduced with 0.4.11 with synthetic paths [#215](https://github.com/databricks/sjsonnet/pull/215)
- Fix thread-safety bug in Obj.getAllKeys [#217](https://github.com/databricks/sjsonnet/pull/217)

### 0.4.11
- Implement `std.isEmpty`, `std.xor`, `std.xnor`, `std.trim`,
  `std.equalsIgnoreCase`, `std.sha1`, `std.sha256`, `std.sha512`, `std.sha3` [#204](https://github.com/databricks/sjsonnet/pull/210)
- fix: std.manifestJsonMinified and empty arrays/objects [#207](https://github.com/databricks/sjsonnet/pull/207)
- fix: Use different chars for synthetic paths. [#208](https://github.com/databricks/sjsonnet/pull/208)
- Fix sorting algorithm to work for all array types [#211](https://github.com/databricks/sjsonnet/pull/211)
- Add better error handling for format [#212](https://github.com/databricks/sjsonnet/pull/212)

### 0.4.10

- Implement `std.get` [#202](https://github.com/databricks/sjsonnet/pull/202), 
  `std.all` and `std.any` [#203](https://github.com/databricks/sjsonnet/pull/203)

### 0.4.9

- Switch from CRC32 to XXHash64 for import cache keys [#198](https://github.com/databricks/sjsonnet/pull/198) 

### 0.4.8

- Significant reduction in memory usage from importing and parsing large files [#194](https://github.com/databricks/sjsonnet/pull/194) [#197](https://github.com/databricks/sjsonnet/pull/197)

### 0.4.7

- Ensure almost-octal-number-like strings are quoted when `--yaml-out` is passed,
  to avoid issues with non-compliant YAML parsers [#183](https://github.com/databricks/sjsonnet/pull/183)

### 0.4.6

- Add `std.parseYaml`

### 0.4.5

- Make Jsonnet standard library configurable and non-global
  [#166](https://github.com/databricks/sjsonnet/pull/166), fixing a race condition
  on library initialization in multithreaded environments and allowing custom `std.*`
  functions to be passed in by the user

- Added a flag `--no-duplicate-keys-in-comprehension` to follow upstream google/jsonnet
  behavior of failing if a dictionary comprehension
  [#156](https://github.com/databricks/sjsonnet/pull/156)
  [ea8720f](https://github.com/databricks/sjsonnet/commit/ea8720f218a4765b64e6313ac9f4a84d99c6e315).
  This is optional for migration purposes but will likely become the default in future

- Disallow Jsonnet identifiers that start with numbers [#161](https://github.com/databricks/sjsonnet/pull/161)

- Fix parsing of `+:` in dictionary comprehensions [#155](https://github.com/databricks/sjsonnet/pull/155)

- Fix parse failure when passing `id == ...` to a function arguments
  [#151](https://github.com/databricks/sjsonnet/pull/151)

- Add a flag `--strict-import-syntax` to disallow the syntax `import "foo".bar`,
  when it should be `(import "foo").bar`, following upstream google/jsonnet.
  This is optional for migration purposes but will likely become the default in future
  [#153](https://github.com/databricks/sjsonnet/pull/153)
  [ccbe6e](https://github.com/databricks/sjsonnet/commit/ccbe6e3a3f8cfb661ab6fa733aff690a61e47022)

- Allow assertions to take non-string error messages [#170](https://github.com/databricks/sjsonnet/pull/170)

- Fix parsing of object keys starting with the prefix `assert`
  [#169](https://github.com/databricks/sjsonnet/pull/169)

- Properly propagate assertions during inheritance [#172](https://github.com/databricks/sjsonnet/pull/172),
  and add the flag `--strict-inherited-assertions` to fix a bug where assertions
  inherited from a shared object were only triggering once, rather than
  once-per-inheritor [95342](https://github.com/databricks/sjsonnet/commit/9534260fff4a50d29db379e307bce9a484790fa7).
  This is optional for migration purposes but will likely become the default in future

- Add `std.slice`, `std.manifestJsonMinified`, fix handling of numbers in
  `manifestXmlJsonml`, handling of code in `extCode`
  [#171](https://github.com/databricks/sjsonnet/pull/171)

- Add the ability to include code in `--tla-code` and `--tla-code-file`
  [#175](https://github.com/databricks/sjsonnet/pull/175)

- Add `std.reverse` [a425342](https://github.com/databricks/sjsonnet/commit/a42534244c6966d049f38167473339be899d11af)

- Fixes to main method handling of various combinations of `--exec`, `--yaml-out`,
  `-yaml-stream`, `--multi`, and `--output-file` [#174](https://github.com/databricks/sjsonnet/pull/174)

### 0.4.4

- Update Mill to 0.10.12
- Fix parsing of k/v cli arguments with an "=" in the value

### 0.4.2

- Make lazy initialization of static Val.Obj thread-safe [#136](https://github.com/databricks/sjsonnet/pull/136)
- Deduplicate strings in the parser [#137](https://github.com/databricks/sjsonnet/pull/137)
- Update the JS example [#141](https://github.com/databricks/sjsonnet/pull/141)

### 0.4.1

- Additional significant performance improvements [#119](https://github.com/databricks/sjsonnet/pull/119)
- Error handling fixes and improvements [#125](https://github.com/databricks/sjsonnet/pull/125)

### 0.4.0

- Performance improvements with lots of internal changes [#117](https://github.com/databricks/sjsonnet/pull/117)

### 0.3.3

- Bump uJson version to 1.3.7

### 0.3.2

- Bump uJson version to 1.3.0

### 0.3.1

- Avoid catching fatal exceptions during evaluation

### 0.3.0

- Add `--yaml-debug` flag to add source-line comments showing where each line of YAML came from [#105]()https://github.com/databricks/sjsonnet/pull/105
- Add `objectValues` and `objectVlauesAll` to stdlib [#104](https://github.com/databricks/sjsonnet/pull/104)

### 0.2.8

- Allow direct YAML output generation via `--yaml-out`
- Do not allow duplicate field in object when evaluating list list comprehension [#100](https://github.com/databricks/sjsonnet/pull/100)
- Fix compiler crash when '+' signal is true in a field declaration inside a list comprehension [#98](https://github.com/databricks/sjsonnet/pull/98)
- Fix error message for too many arguments with at least one named arg [#97](https://github.com/databricks/sjsonnet/pull/97)

### 0.2.7

- Streaming JSON output to disk for lower memory usage [#85](https://github.com/databricks/sjsonnet/pull/85)
- Static detection of duplicate fields [#86](https://github.com/databricks/sjsonnet/pull/86)
- Strict mode to disallow error-prone adjacent object literals [#88](https://github.com/databricks/sjsonnet/pull/88)

### 0.2.6

- Add `std.flatMap`, `std.repeat`, `std.clamp`, `std.member`, `std.stripChars`, `std.rstripChars`, `std.lstripChars`

### 0.2.4

- Add support for syntactical key ordering [#53](https://github.com/databricks/sjsonnet/pull/53)
- Bump dependency versions

### 0.2.2

- Bump verion of Scalatags, uPickle

### 0.1.9

- Bump version of FastParse

### 0.1.8

- Bump versions of OS-Lib, uJson, Scalatags

### 0.1.7

- Support std lib methods that take a key lambda [#40](https://github.com/databricks/sjsonnet/pull/40)
- Handle hex in unicode escaoes [#41](https://github.com/databricks/sjsonnet/pull/41)
- Add encodeUTF8, decodeUTF8 std lib methdos [#42](https://github.com/databricks/sjsonnet/pull/42)
- Properly fail on non-boolean conditionals [#44](https://github.com/databricks/sjsonnet/pull/44)
- Support YAML-steam output [#45](https://github.com/databricks/sjsonnet/pull/45)

### 0.1.6

- ~2x performance increase

### 0.1.5

- Javascript support, allowing Sjsonnet to be used in the browser or on
  Node.js
- Performance improvements

### 0.1.4

- Scala 2.13 support
- Performance improvements

### 0.1.3

- Add `std.mod`, `std.min` and `std.max`
- Performance improvements

### 0.1.2

- Improvements to error reporting when types do not match

### 0.1.1

- Performance improvements to the parser via upgrading to Fastparse 2.x

### 0.1.0

- First release
