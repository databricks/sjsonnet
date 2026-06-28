{
  // round(-0) should preserve negative zero sign
  roundNegZero: std.round(-0),
  // round(0) should stay positive zero
  roundPosZero: std.round(0),
  // round(-0.4) should produce -0
  roundNegSmall: std.round(-0.4),
  // round(0.5) should produce 1
  roundHalfUp: std.round(0.5),
  // round(-0.5) should produce -1
  roundNegHalf: std.round(-0.5),
  // atan2 verifies sign of zero
  signNegZero: std.atan2(std.round(-0), -1) < 0,
  signPosZero: std.atan2(std.round(0), -1) > 0,
}
