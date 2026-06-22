// Regression test for large float formatting with %d/%o/%x/%f
// Format by truncating the IEEE 754 value directly instead of relying on
// platform BigDecimal(double) implementations.
local test_d_1e30 = ("%d" % 1e30) == "1000000000000000019884624838656";
local test_d_neg_1e30 = ("%d" % -1e30) == "-1000000000000000019884624838656";
local test_d_1e20 = ("%d" % 1e20) == "100000000000000000000";
local test_d_1_5e20 = ("%d" % 1.5e20) == "150000000000000000000";
local test_x_1e20 = ("%x" % 1e20) == "56bc75e2d63100000";
local test_o_1e20 = ("%o" % 1e20) == "12657072742654304000000";
local test_d_2pow63 = ("%d" % 9223372036854775808) == "9223372036854775808";
local test_x_2pow63 = ("%x" % 9223372036854775808) == "8000000000000000";
local test_o_2pow63 = ("%o" % 9223372036854775808) == "1000000000000000000000";
local test_f0_1e30 = ("%.0f" % 1e30) == "1000000000000000019884624838656";
local test_f0_1e25 = ("%.0f" % 1e25) == "10000000000000000905969664";
local test_f0_2pow63 = ("%.0f" % 9223372036854775808) == "9223372036854775808";
local test_d_2pow53 = ("%d" % 9007199254740992) == "9007199254740992";
local test_d_2pow53_plus1 = ("%d" % 9007199254740993) == "9007199254740992";
local results = [
  ["%d 1e30", test_d_1e30],
  ["%d -1e30", test_d_neg_1e30],
  ["%d 1e20", test_d_1e20],
  ["%d 1.5e20", test_d_1_5e20],
  ["%x 1e20", test_x_1e20],
  ["%o 1e20", test_o_1e20],
  ["%d 2^63", test_d_2pow63],
  ["%x 2^63", test_x_2pow63],
  ["%o 2^63", test_o_2pow63],
  ["%.0f 1e30", test_f0_1e30],
  ["%.0f 1e25", test_f0_1e25],
  ["%.0f 2^63", test_f0_2pow63],
  ["%d 2^53", test_d_2pow53],
  ["%d 2^53+1", test_d_2pow53_plus1],
];
local failures = [r[0] for r in results if !r[1]];
if std.length(failures) > 0 then
  error "Failed: " + std.join(", ", failures)
else
  "All large float format tests passed"
