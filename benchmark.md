## Environment
Run in one thread with jmh, and measure the operations per second.

## Result

| Version  | Ops
|---|---
| 0.1.3   | 2000
| cache evaluate result | 2246
| remove os.path.relative   | 3712
| *make obj.value return Val** | 3832
| *make obj.Array non lazy**   | 4156
| use java.nio.relative path    | 3359
| convert object to ujson   | 3637

_*_ Notes: Two version above improve the overall performance, but break the
 laziness semantic of the jsonnet language. 