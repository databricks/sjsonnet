std.assertEqual(std.format("%.16f", 3.14159265358979323), "3.1415926535897931") &&
std.assertEqual(std.format("%.17f", 1.0 / 3.0), "0.33333333333333331") &&
std.assertEqual(std.format("%.17f", 3.14159265358979323), "3.14159265358979312") &&
std.assertEqual(std.format("%.17f", 0.1), "0.10000000000000001") &&
std.assertEqual(std.format("%.17g", 3.14159265358979323), "3.1415926535897931") &&
std.assertEqual(std.format("%.17g", 31.4159265358979323), "31.415926535897931") &&
std.assertEqual(std.format("%.16f", 1.00000762939453125), "1.0000076293945312") &&
std.assertEqual(std.format("%.16f", 1.00002288818359375), "1.0000228881835938") &&
std.assertEqual(std.format("%.16f", -1.00000762939453125), "-1.0000076293945312") &&
std.assertEqual(std.format("%.17g", 1.00000762939453125), "1.0000076293945312") &&
std.assertEqual(std.format("%.15f", 3.14159265358979323), "3.141592653589793") &&
std.assertEqual(std.format("%.20f", 0.1), "0.10000000000000000555") &&
std.assertEqual(std.format("%.20f", 1e-10), "0.00000000010000000000") &&
std.assertEqual(std.format("%.16f", 5e-324), "0.0000000000000000") &&
std.assertEqual(std.format("%.20g", 0.1), "0.10000000000000000555") &&
true
