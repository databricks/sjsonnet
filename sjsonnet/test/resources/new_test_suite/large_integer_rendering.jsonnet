// Test large integer-valued doubles render correctly
// Covers integers beyond Int.MaxValue (2^31-1 = 2147483647)
local x = 300000000000;
local y = 1000000000000;
local z = 9007199254740992;  // 2^53, max exact integer in double
std.assertEqual(std.toString(x), "300000000000") &&
std.assertEqual(std.toString(y), "1000000000000") &&
std.assertEqual(std.toString(z), "9007199254740992") &&
std.assertEqual(std.toString(-x), "-300000000000") &&
std.assertEqual(std.toString(-y), "-1000000000000") &&
std.assertEqual(std.toString(0), "0") &&
std.assertEqual(std.toString(2147483647), "2147483647") &&
std.assertEqual(std.toString(2147483648), "2147483648") &&
true
