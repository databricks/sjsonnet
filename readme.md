# Sjsonnet

A JVM implementation of the [Jsonnet](https://jsonnet.org/) configuration
language.

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

## Laziness

The Jsonnet language is *lazy*: expressions don't get evaluated unless their
value is needed, and thus even erroneous expressions do not cause a failure if
un-used. This is represented in the Sjsonnet codebase by `sjsonnet.Lazy`: a
wrapper type that encapsulates an arbitrary computation that returns a
`sjsonnet.Val`.

`sjsonnet.Lazy` is used in several places, representing where laziness is
present in the language:

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
