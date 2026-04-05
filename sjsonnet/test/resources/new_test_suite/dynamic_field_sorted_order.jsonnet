// Verify that objects with dynamic field names are sorted correctly
// when the same MemberList AST produces different key sets across invocations.
// Regression test for materializer sorted-cache correctness.
local mk(x) = {[x]: 1, a: 2};
std.assertEqual(mk("0"), {"0": 1, a: 2}) &&
std.assertEqual(mk("z"), {a: 2, z: 1}) &&
// Verify multiple calls with different dynamic keys still sort correctly
local results = [mk("c"), mk("b"), mk("d")];
std.assertEqual(results[0], {a: 2, c: 1}) &&
std.assertEqual(results[1], {a: 2, b: 1}) &&
std.assertEqual(results[2], {a: 2, d: 1}) &&
true
