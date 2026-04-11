// Regression test: ensure signed zero (-0.0) is preserved through arithmetic
// and is not canonicalized to +0.0 by the integer cache.
// Observable via atan2: atan2(-0.0, -1) = -π, atan2(+0.0, -1) = +π
local neg_zero = 0 * -1;
local pos_zero = 0;

std.assertEqual(std.sign(neg_zero), 0) &&
std.assertEqual(std.sign(pos_zero), 0) &&
// atan2 is the canonical way to distinguish -0.0 from +0.0
std.assertEqual(std.atan2(neg_zero, -1) < 0, true) &&
std.assertEqual(std.atan2(pos_zero, -1) > 0, true) &&
true
