// A doubled comma is not a trailing comma: `[1,,]` must be a parse error
// (go-jsonnet, C++ jsonnet and jrsonnet all reject it).
[1,,]
