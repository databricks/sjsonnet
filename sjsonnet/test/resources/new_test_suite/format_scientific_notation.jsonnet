// Regression test: scientific notation for exact powers of 10 and numbers < 1
// Previously produced wrong output like "10.000000e+-04" for 0.001
std.assertEqual(std.format("%e", 0.001), "1.000000e-03") &&
std.assertEqual(std.format("%e", 1.0), "1.000000e+00") &&
std.assertEqual(std.format("%e", 100.0), "1.000000e+02") &&
std.assertEqual(std.format("%e", 0.1), "1.000000e-01") &&
std.assertEqual(std.format("%E", 1234.5), "1.234500E+03") &&
std.assertEqual(std.format("%e", 0), "0.000000e+00") &&
true
