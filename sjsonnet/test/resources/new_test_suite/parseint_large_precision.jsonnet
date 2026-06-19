// Verify that parseInt/parseHex/parseOctal produce correct IEEE 754 doubles
// for large integers near the precision boundary (2^53).
// go-jsonnet parses to int64 first then converts to float64 for correct rounding.
{
  // 16-digit: within safe integer range
  parseInt_16: std.parseInt("1234567890123456"),
  // 17-digit: near precision boundary
  parseInt_17: std.parseInt("12345678901234567"),
  // 18-digit: beyond Long range, should round to nearest double
  parseInt_18: std.parseInt("999999999999999999"),
  // Max safe integer
  parseInt_max_safe: std.parseInt("9007199254740991"),
  // Just beyond safe integer
  parseInt_over_safe: std.parseInt("9007199254740993"),
  // Power of 10
  parseInt_1e18: std.parseInt("1000000000000000000"),
  // Negative large
  parseInt_neg_big: std.parseInt("-999999999999999999"),
  // Hex: max unsigned 64-bit
  parseHex_max_u64: std.parseHex("FFFFFFFFFFFFFFFF"),
  // Hex: 32-bit max
  parseHex_u32: std.parseHex("FFFFFFFF"),
  // Octal: large value
  parseOct_big: std.parseOctal("1000000000000000000000"),
}
