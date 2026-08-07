// A lone comma is not a parameter list: `function(,)` must be a parse error
// (go-jsonnet and jrsonnet both reject it).
(function(,) 1)()
