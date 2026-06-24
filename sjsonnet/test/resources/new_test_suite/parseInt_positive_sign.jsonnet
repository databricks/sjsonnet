// Test that std.parseInt accepts + prefix like go-jsonnet.
assert std.parseInt("+123") == 123 : "parseInt(+123) should return 123";
assert std.parseInt("+0") == 0 : "parseInt(+0) should return 0";
assert std.parseInt("+007") == 7 : "parseInt(+007) should return 7";
assert std.parseInt("+1") == 1 : "parseInt(+1) should return 1";
assert std.parseInt("-123") == -123 : "parseInt(-123) should still work";
assert std.parseInt("123") == 123 : "parseInt(123) should still work";
true
