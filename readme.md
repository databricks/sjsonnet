# Sjsonnet
A Scala implementation of the [Jsonnet](https://jsonnet.org/) configuration language, running on JVM, GraalVM, Scala Native and JavaScript.

## Usage as a CLI
We release standalone executables JARs, Scala Native and GraalVM in the [github release page](https://github.com/databricks/sjsonnet/releases):

```bash
$ chmod +x sjsonnet.jar

$ ./sjsonnet.jar
Missing argument: file <str>
Expected Signature: Sjsonnet
usage: sjsonnet [sjsonnet-options] script-file
  -A --tla-str <str>              <var>[=<val>] Provide top-level arguments as string. 'If <val> is
                                  omitted, get from environment var <var>
  -J --jpath <str>                Specify an additional library search dir (left-most wins unless
                                  reverse-jpaths-priority is set)
  -S --string                     Expect a string, manifest as plain text
  -V --ext-str <str>              <var>[=<val>] Provide 'external' variable as string. 'If <val> is
                                  omitted, get from environment var <var>
  -V --ext-code <str>             <var>[=<code>] Provide 'external' variable as Jsonnet code. If
                                  <code> is omitted, get from environment var <var>
  -V --tla-code <str>             <var>[=<val>] Provide top-level arguments as Jsonnet code. 'If
                                  <val> is omitted, get from environment var <var>
  -c --create-output-dirs         Automatically creates all parent directories for files
  --debug-importer                Print some additional debugging information about the importer
  -e --exec                       Evaluate the given string as Jsonnet rather than treating it as a
                                  file name
  --ext-code-file <str>           <var>=<file> Provide 'external' variable as Jsonnet code from the
                                  file
  --ext-str-file <str>            <var>=<file> Provide 'external' variable as string from the file
  --fatal-warnings                Fail if any warnings were emitted
  file <str>                      The jsonnet file you wish to evaluate
  -m --multi <str>                Write multiple files to the directory, list files on stdout
  -n --indent <int>               How much to indent your output JSON
  -o --output-file <str>          Write to the output file rather than stdout
  -p --preserve-order             Preserves order of keys in the resulting JSON
  --reverse-jpaths-priority       If set, reverses the import order of specified jpaths (so that the
                                  rightmost wins)
  --strict                        Enforce some additional syntax limitations
  --throw-error-for-invalid-sets  Throw an error if a set operation is used on a non-set
  --tla-code-file <str>           <var>=<file> Provide top-level arguments variable as Jsonnet code
                                  from the file
  --tla-str-file <str>            <var>=<file> Provide top-level arguments variable as string from
                                  the file
  -y --yaml-stream                Write output as a YAML stream of JSON documents
  --yaml-debug                    Generate source line comments in the output YAML doc to make it
                                  easier to figure out where values come from.
  --yaml-out                      Write output as a YAML document

$ ./sjsonnet.jar foo.jsonnet
```


## Usage as a library

Sjsonnet can be used from Java and Scala:

```xml
<dependency>
    <groupId>com.databricks</groupId>
    <artifactId>sjsonnet_3</artifactId>
    <version>${sjsonnet.version}</version>
</dependency>
```

```java
// Java
sjsonnet.SjsonnetMain.main0(
    new String[]{"foo.jsonnet"},
    new DefaultParseCache,
    System.in,
    System.out,
    System.err,
    os.package$.MODULE$.pwd(),
    scala.None$.empty()
);

// Scala
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

Or from Javascript:

Since `0.5.3`, the output is a CommonJS module.

```javascript
const { SjsonnetMain } = require("./sjsonnet.js");
// or
import { SjsonnetMain } from "./sjsonnet";

console.log(
  SjsonnetMain.interpret("local f = function(x) x * x; f(11)", {}, {}, "", (wd, imported) => null)
);
// => 121

console.log(
  SjsonnetMain.interpret(
    "local f = import 'foo'; f + 'bar'", // code
    {}, // extVars
    {}, // tlaVars
    "", // initial working directory

    // resolver callback: receives a base directory and the imported path string,
    // returns the resolved path
    (wd, imported) => wd + "/" + imported,
    // loader callback: receives the full path and returns the file contents
    (path, binary) => "local bar = 123; bar + bar"
  )
);
// => '246bar'
```

Note that since Javascript does not necessarily have access to the
filesystem, you have to provide an explicit import callback that you can
use to resolve imports yourself (whether through Node's `fs` module, or
by emulating a filesystem in-memory)

### Running deeply recursive Jsonnet programs

The depth of recursion is limited by running environment stack size. You can run Sjsonnet with increased
stack size as follows:

```bash
# JVM
java -Xss100m -cp sjsonnet.jar sjsonnet.SjsonnetMain foo.jsonnet

# Scala Native
SCALANATIVE_THREAD_STACK_SIZE=100m ./sjsonnet foo.jsonnet

# ScalaJS (Node)
node --stack-size=100m
```

## Architecture

Sjsonnet is implemented as an optimizing interpreter. There are roughly 4
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

|                                                 | Sjsonnet 0.4.3 | google/go-jsonnet 0.20.0 | google/jsonnet 0.20.0 |
|:------------------------------------------------|---------------:|-------------------------:|----------------------:|
| staging/runbot-app.jsonnet (~6.6mb output JSON) |         ~0.10s |                    ~6.5s |                  ~67s |

Sjsonnet was run as a long-lived daemon to keep the JVM warm,
while go-jsonnet and google/jsonnet were run as subprocesses, following typical
usage patterns. The Sjsonnet command
line which is run by all of these is defined in
`MainBenchmark.mainArgs`. You need to change it to point to a suitable input
before running a benchmark or the profiler.

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
builtin functions, other implementations [google/go-jsonnet](https://github.com/google/go-jsonnet/)
and [jrsonnet](https://github.com/CertainLach/jrsonnet) implement the Jsonnet standard library in the host language too.

## Client-Server

Sjsonnet comes with a built in thin-client and background server, to help
mitigate the unfortunate JVM warmup overhead that adds ~1s to every invocation
down to 0.2-0.3s. For the simple non-client-server executable, you can use

```bash
./mill -i show sjsonnet[3.3.7].jvm.assembly
```

To create the executable. For the client-server executable, you can use

```bash
./mill -i show sjsonnet[3.3.7].server.assembly
```

By default, the Sjsonnet background server lives in `~/.sjsonnet`, and lasts 5
minutes before shutting itself when inactive.

Since the Sjsonnet client still has 0.2-0.3s of overhead, if using Sjsonnet
heavily it is still better to include it in your JVM classpath and invoke it
programmatically via `new Interpreter(...).interpret(...)`.

## Publishing

To publish the JVM version to Maven, make sure the version number in `build.mill` is correct, then run the following commands:
```bash
./mill -i mill.scalalib.SonatypeCentralPublishModule/publishAll \
    --username $SONATYPE_USER --password $SONATYPE_PASSWORD --publishArtifacts __.publishArtifacts \
    --gpgArgs --passphrase=$GPG_PASSPHRASE,--batch,--yes,-a,-b,--pinentry-mode=loopback
```
