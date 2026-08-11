// Jsonnet documents std.format as Python %-formatting. Round the exact binary64 input once,
// using round-to-nearest with ties-to-even.

// Exact halfway values at integer precision
std.assertEqual(std.format("%.0f", [0.5]), "0") &&
std.assertEqual(std.format("%.0f", [1.5]), "2") &&
std.assertEqual(std.format("%.0f", [2.5]), "2") &&
std.assertEqual(std.format("%.0f", [3.5]), "4") &&
std.assertEqual(std.format("%.0f", [4.5]), "4") &&
std.assertEqual(std.format("%.0f", [-0.5]), "-0") &&
std.assertEqual(std.format("%.0f", [-1.5]), "-2") &&
std.assertEqual(std.format("%.0f", [-2.5]), "-2") &&

// Exact halfway values and decimal-looking values at fractional precision
std.assertEqual(std.format("%.1f", [0.25]), "0.2") &&
std.assertEqual(std.format("%.1f", [0.35]), "0.3") &&
std.assertEqual(std.format("%.1f", [-0.25]), "-0.2") &&
std.assertEqual(std.format("%.2f", [0.125]), "0.12") &&
std.assertEqual(std.format("%.2f", [0.375]), "0.38") &&
std.assertEqual(std.format("%.2f", [0.005]), "0.01") &&
std.assertEqual(std.format("%.2f", [0.015]), "0.01") &&
std.assertEqual(std.format("%.2f", [-0.005]), "-0.01") &&
std.assertEqual(std.format("%.2f", [1.005]), "1.00") &&
std.assertEqual(std.format("%.2f", [1.015]), "1.01") &&
std.assertEqual(std.format("%.2f", [1.025]), "1.02") &&
std.assertEqual(std.format("%.2f", [0.145]), "0.14") &&
std.assertEqual(std.format("%.2f", [2.675]), "2.67") &&
std.assertEqual(std.format("%.2f", [-2.675]), "-2.67") &&

// Scientific and generic conversions use the same exact-value rounding rule
std.assertEqual(std.format("%.0e", [2.5]), "2e+00") &&
std.assertEqual(std.format("%.0e", [-2.5]), "-2e+00") &&
std.assertEqual(std.format("%.2e", [1.005]), "1.00e+00") &&
std.assertEqual(std.format("%.1g", [0.25]), "0.2") &&
std.assertEqual(std.format("%.1g", [2.5]), "2") &&
std.assertEqual(std.format("%.1g", [25]), "2e+01") &&
std.assertEqual(std.format("%.1g", [-25]), "-2e+01") &&
std.assertEqual(std.format("%.3g", [1.005]), "1") &&
std.assertEqual(std.format("%.3g", [2.675]), "2.67") &&

// Carry case: rounding causes integer part to increment
std.assertEqual(std.format("%.2f", [9.999]), "10.00") &&
// Negative rounding to zero
std.assertEqual(std.format("%.2f", [-0.001]), "-0.00") &&
// Large-integer regression: |x| >= 2^52, ULP >= 1.0, must be identity
std.assertEqual(std.format("%.0f", [9007199254740991]), "9007199254740991") &&
std.assertEqual(std.format("%.0f", [-9007199254740991]), "-9007199254740991") &&
std.assertEqual(std.format("%.0f", [4503599627370497]), "4503599627370497") &&
std.assertEqual(std.format("%.0f", [1e20]), "100000000000000000000") &&
true
