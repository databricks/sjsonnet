// Trailing commas remain valid in objects, arrays, calls and parameter lists.
local f(a, b,) = [a, b];
local g(x = 1,) = x;
std.assertEqual({a: 1, b: 2,}, {a: 1, b: 2}) &&
std.assertEqual([1, 2,], [1, 2]) &&
std.assertEqual(f(1, 2,), [1, 2]) &&
std.assertEqual(g(), 1) &&
std.assertEqual({}, {})
