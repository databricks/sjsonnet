// A lone comma is not an argument list: `f(,)` must be a parse error
// (go-jsonnet and jrsonnet both reject it).
local f(a) = a;
f(,)
