// Test that std.manifestJsonEx respects the actual indent characters,
// not just the string length.

// Tab indent should use actual tab characters
local tab_result = std.manifestJsonEx({a: 1, b: [2, 3]}, "\t");
local two_space = std.manifestJsonEx({a: 1}, "  ");
local three_space = std.manifestJsonEx({a: 1}, "   ");
local empty_indent = std.manifestJsonEx({a: {b: "c"}}, "", " ", " : ");

std.assertEqual(
  std.manifestJsonEx({a: 1}, "\t"),
  "{\n\t\"a\": 1\n}"
) &&
std.assertEqual(
  std.manifestJsonEx({a: 1, b: [2]}, "\t"),
  "{\n\t\"a\": 1,\n\t\"b\": [\n\t\t2\n\t]\n}"
) &&
std.assertEqual(
  std.manifestJsonEx({a: 1}, "    "),
  "{\n    \"a\": 1\n}"
) &&
std.assertEqual(
  std.manifestJsonEx({a: 1}, "-"),
  "{\n-\"a\": 1\n}"
) &&
std.assertEqual(
  empty_indent,
  "{ \"a\" : { \"b\" : \"c\" } }"
) &&
true
