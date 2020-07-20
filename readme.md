# Sjsonnet

A JVM implementation of the [Jsonnet](https://jsonnet.org/) configuration
language.

## Usage

Sjsonnet can be used from Java:

```xml
<dependency>
    <groupId>com.lihaoyi</groupId>
    <artifactId>sjsonnet_2.13</artifactId>
    <version>0.2.6</version>
</dependency>
```

```java
sjsonnet.SjsonnetMain.main0(
    new String[]{"foo.jsonnet"},
    sjsonnet.SjsonnetMain.createParseCache(),
    System.in,
    System.out,
    System.err,
    os.package$.MODULE$.pwd(),
    scala.None$.empty()
);
```

From Scala:

```scala
"com.lihaoyi" %% "sjsonnet" % "0.2.6" // SBT
ivy"com.lihaoyi::sjsonnet:0.2.6" // Mill
```

```scala
sjsonnet.SjsonnetMain.main0(
    Array("foo.jsonnet"),
    sjsonnet.SjsonnetMain.createParseCache(),
    System.in,
    System.out,
    System.err,
    os.pwd, // working directory
    None
);
```

As a standalone executable assembly:

- <https://github.com/lihaoyi/sjsonnet/releases/download/0.2.6/sjsonnet.jar>

```bash
$ curl -L https://github.com/lihaoyi/sjsonnet/releases/download/0.2.6/sjsonnet.jar > sjsonnet.jar

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
$ curl -L https://github.com/databricks/sjsonnet/releases/download/0.2.6/sjsonnet.js > sjsonnet.js

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
    // returns a 2-element array of the resolved file path and file contents
    (wd, imported) => [wd + "/" + imported, "local bar = 123; bar + bar"])
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

Sjsonnet is implementated as a straightforward AST interpreter. There are
roughly 3 phases:

- `sjsonnet.Parser`: parses an input `String` into a `sjsonnet.Expr`, which is a
  [Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) representing
  the Jsonnet document syntax, using the
  [Fastparse](https://github.com/lihaoyi/fastparse) parsing library

- `sjsonnet.Evaluator`: recurses over the `sjsonnet.Expr` and converts it into a
  `sjsonnet.Val`, a data structure representing the Jsonnet runtime values
  (basically lazy JSON which can contain function values).

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
  pass around functions as values and call then later on. The second is that and
  object values & array entries are *lazy*: e.g. `[error 123, 456][1]` does not
  raise an error because the first (erroneous) entry of the array is un-used and
  thus not evaluated.

## Performance

Due to pervasive caching, sjsonnet is much faster than google/jsonnet. See
this blog post for more details:

- [Writing a Faster Jsonnet Compiler](https://databricks.com/blog/2018/10/12/writing-a-faster-jsonnet-compiler.html)

Here's the latest set of benchmarks I've run comparing Sjsonnet against
google/jsonnet and google/go-jsonnet, measuring the time taken to  
evaluate the `test_suite/` folder (smaller is better):

|              | Sjsonnet 0.1.5 | Sjsonnet 0.1.6 |
|:-------------|---------------:|---------------:|
| Scala 2.13.0 | 14.26ms ± 0.22 |  6.59ms ± 0.27 |
| Scala 2.12.8 | 18.07ms ± 0.30 |  9.29ms ± 0.26 |

| google/jsonnet | google/go-jsonnet |
|---------------:|------------------:|
|         ~1277ms|             ~274ms|

google/jsonnet was built from source on commit
f59758d1904bccda99598990f582dd2e1e9ad263, while google/go-jsonnet was
`go get`ed at version `v0.13.0`. You can see the source code of the
benchmark in  
[SjsonnetTestMain.scala](https://github.com/databricks/sjsonnet/blob/master/sjsonnet/test/src-jvm/sjsonnet/SjsonnetTestMain.scala)

## Laziness

The Jsonnet language is *lazy*: expressions don't get evaluated unless
their value is needed, and thus even erroneous expressions do not cause
a failure if un-used. This is represented in the Sjsonnet codebase by
`sjsonnet.Val.Lazy`: a wrapper type that encapsulates an arbitrary
computation that returns a `sjsonnet.Val`.

`sjsonnet.Val.Lazy` is used in several places, representing where
laziness is present in the language:

- Inside `sjsonnet.Scope`, representing local variable name bindings

- Inside `sjsonnet.Val.Arr`, representing the contents of array cells

- Inside `sjsonnet.Val.Obj`, representing the contents of object values

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
./mill show sjsonnet[2.13.0].assembly
```

To create the executable. For the client-server executable, you can use

```bash
./mill show sjsonnet[2.13.0].server.assembly
```

By default, the Sjsonnet background server lives in `~/.sjsonnet`, and lasts 5
minutes before shutting itself when inactive.

Since the Sjsonnet client still has 0.2-0.3s of overhead, if using Sjsonnet
heavily it is still better to include it in your JVM classpath and invoke it
programmatically via `new Interpreter(...).interpret(...)`.

## Publishing

To publish, run the following commands:

```bash
./mill -i mill.scalalib.PublishModule/publishAll lihaoyi:$SONATYPE_PASSWORD $GPG_PASSWORD --publishArtifacts __.publishArtifacts --release true

./mill -i show sjsonnet[2.13.0].js.fullOpt
./mill -i show sjsonnet[2.13.0].jvm.assembly
```

## Changelog

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
