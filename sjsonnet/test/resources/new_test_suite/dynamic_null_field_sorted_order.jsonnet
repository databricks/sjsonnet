// Verify that objects with conditional (nullable) field names
// produce correct output when the same MemberList creates objects
// with different field counts across invocations.
// Regression test for materializer sorted-cache + null field correctness.
local f(x) = { [x]: 1, [if x == "a" then "c"]: 2, b: 3 };
// First call: x="a", both dynamic fields present → 3 fields
std.assertEqual(f("a"), {a: 1, b: 3, c: 2}) &&
// Second call: x="z", second dynamic field null → 2 fields
std.assertEqual(f("z"), {b: 3, z: 1}) &&
true
