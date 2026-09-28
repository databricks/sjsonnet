// Per the Jsonnet spec, lhs { body } is equivalent to lhs + { body }.
local identity(x) = x;
local base = { x: 1, y: self.x };

std.assertEqual("hello" { x: 1 }, 'hello{"x": 1}') &&
std.assertEqual(identity("hello") {}, "hello" + {}) &&
std.assertEqual("" { local x = 2, x: x, y: self.x }, '{"x": 2, "y": 2}') &&
std.assertEqual("" { x: 1, nested: { y: $.x } }, '{"nested": {"y": 1}, "x": 1}') &&
std.assertEqual("" { hidden:: error "unused", x: 1 }, '{"x": 1}') &&
std.assertEqual("" { assert self.x == 1, x: 1 }, '{"x": 1}') &&
std.assertEqual("" { x+: 1, hasSuper: "x" in super }, '{"hasSuper": false, "x": 1}') &&
std.assertEqual("" { [null]: error "unused" }, "" + {}) &&
std.assertEqual("" { [k]: k for k in ["z", "a"] }, '{"a": "a", "z": "z"}') &&
std.assertEqual("" { [k]: error "unused" for k in [] }, "" + {}) &&
std.assertEqual("" { [k]: error "unused" for k in [null] }, "" + {}) &&
std.assertEqual("hello" { x: 1 } { y: 2 }, 'hello{"x": 1}{"y": 2}') &&
std.assertEqual(base { x: 2, z: super.x }, { x: 2, y: 2, z: 1 }) &&
std.assertEqual(
  std.foldl(function(acc, x) acc { x: x }, [1, 2], "hello"),
  'hello{"x": 1}{"x": 2}'
) &&
true
