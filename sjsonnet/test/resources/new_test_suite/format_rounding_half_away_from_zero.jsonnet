// Verify std.format uses round-half-away-from-zero (matching go-jsonnet/jrsonnet).
// Previously used HALF_EVEN (banker's rounding) which gave wrong results for .5 values.

std.assertEqual(std.format("%.0f", [0.5]), "1") &&
std.assertEqual(std.format("%.0f", [1.5]), "2") &&
std.assertEqual(std.format("%.0f", [2.5]), "3") &&
std.assertEqual(std.format("%.0f", [3.5]), "4") &&
std.assertEqual(std.format("%.0f", [4.5]), "5") &&
std.assertEqual(std.format("%.0f", [-0.5]), "-1") &&
std.assertEqual(std.format("%.0f", [-1.5]), "-2") &&
std.assertEqual(std.format("%.0f", [-2.5]), "-3") &&
// Higher precision
std.assertEqual(std.format("%.1f", [0.25]), "0.3") &&
std.assertEqual(std.format("%.1f", [0.35]), "0.4") &&
std.assertEqual(std.format("%.1f", [-0.25]), "-0.3") &&
std.assertEqual(std.format("%.2f", [0.005]), "0.01") &&
std.assertEqual(std.format("%.2f", [0.015]), "0.02") &&
std.assertEqual(std.format("%.2f", [-0.005]), "-0.01") &&
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
