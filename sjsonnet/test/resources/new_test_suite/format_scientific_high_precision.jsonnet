std.assertEqual(std.format("%.16e", 3.14159265358979323), "3.1415926535897931e+00") &&
std.assertEqual(std.format("%.17e", 1.0 / 3.0), "3.33333333333333315e-01") &&
std.assertEqual(std.format("%.17e", 9.5), "9.50000000000000000e+00") &&
std.assertEqual(std.format("%.18e", 9.5), "9.500000000000000000e+00") &&
std.assertEqual(std.format("%.19e", 1.5), "1.5000000000000000000e+00") &&
std.assertEqual(std.format("%.19e", 0.1), "1.0000000000000000555e-01") &&
std.assertEqual(std.format("%.20e", 3.14), "3.14000000000000012434e+00") &&
std.assertEqual(std.format("%.19E", -1.5), "-1.5000000000000000000E+00") &&
std.assertEqual(std.format("%.16e", 99.99999999999999), "9.9999999999999986e+01") &&
std.assertEqual(std.format("%.16e", 0.09999999999999999), "9.9999999999999992e-02") &&
std.assertEqual(std.format("%.16e", 1.00000762939453125), "1.0000076293945312e+00") &&
std.assertEqual(std.format("%.16e", -1.00000762939453125), "-1.0000076293945312e+00") &&
std.assertEqual(std.format("%.16e", 0), "0.0000000000000000e+00") &&
std.assertEqual(std.format("%.16e", -0.0), "-0.0000000000000000e+00") &&
true
