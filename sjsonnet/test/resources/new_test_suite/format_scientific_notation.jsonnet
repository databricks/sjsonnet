// Directional tests for std.format scientific notation (%e).
// Verifies correct rounding and high-precision formatting against go-jsonnet.

// Basic scientific notation
local b1 = std.assertEqual(std.format("%e", 0.001), "1.000000e-03");
local b2 = std.assertEqual(std.format("%e", 1.0), "1.000000e+00");
local b3 = std.assertEqual(std.format("%e", 100.0), "1.000000e+02");
local b4 = std.assertEqual(std.format("%e", 0.1), "1.000000e-01");
local b5 = std.assertEqual(std.format("%E", 1234.5), "1.234500E+03");
local b6 = std.assertEqual(std.format("%e", 0), "0.000000e+00");

// Rounding cases (previously truncated instead of rounding)
local r1 = std.assertEqual("%.0e" % 1.5e10, "2e+10");
local r2 = std.assertEqual("%.0e" % 2.5e10, "3e+10");
local r3 = std.assertEqual("%.0e" % 9.5e10, "10e+10");
local r4 = std.assertEqual("%.1e" % 1.55e10, "1.6e+10");
local r5 = std.assertEqual("%.0e" % 1.4e10, "1e+10");
local r6 = std.assertEqual("%.0e" % 1.6e10, "2e+10");

// Edge cases
local e1 = std.assertEqual("%.0e" % 0, "0e+00");
local e2 = std.assertEqual("%.2e" % 0, "0.00e+00");
local e3 = std.assertEqual("%.0e" % (-1.5e10), "-2e+10");
local e4 = std.assertEqual("%.10e" % 1.23456789012e10, "1.2345678901e+10");
local e5 = std.assertEqual("%e" % 1.5e10, "1.500000e+10");
local e6 = std.assertEqual("%.2e" % 1.5e10, "1.50e+10");

b1 && b2 && b3 && b4 && b5 && b6 &&
r1 && r2 && r3 && r4 && r5 && r6 &&
e1 && e2 && e3 && e4 && e5 && e6
