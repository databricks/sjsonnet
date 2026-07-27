std.assertEqual(std.format("100%% done %s", ["yes"]), "100% done yes") &&
std.assertEqual(std.format("%*s", [10, "hello"]), "     hello") &&
std.assertEqual(std.format("%.*f", [3, 3.14159]), "3.142") &&
std.assertEqual(std.format("%#x", [255]), "0xff") &&
true
