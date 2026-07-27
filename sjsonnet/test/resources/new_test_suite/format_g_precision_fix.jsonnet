// Tests for %g and %#g format fixes
// Verifies floating-point precision fixes in roundedGenericExponent and formatGeneric

// Basic %g tests with various exponents
std.assertEqual('%g' % 0.1, '0.1') &&
std.assertEqual('%g' % 0.0001, '0.0001') &&
std.assertEqual('%g' % 1.0, '1') &&
std.assertEqual('%g' % 100.0, '100') &&
std.assertEqual('%g' % 1234567.0, '1.23457e+06') &&

// %#g tests (alternate flag preserves trailing zeros)
std.assertEqual('%#g' % 0.1, '0.100000') &&
std.assertEqual('%#g' % 0.0001, '0.000100000') &&
std.assertEqual('%#g' % 1.0, '1.00000') &&
std.assertEqual('%#g' % 100.0, '100.000') &&
std.assertEqual('%#g' % 1234567.0, '1.23457e+06') &&

// Precision-specific tests
std.assertEqual('%.3g' % 0.0001, '0.0001') &&
std.assertEqual('%.3g' % 123.456, '123') &&
std.assertEqual('%.10g' % 0.1, '0.1') &&

// Zero handling
std.assertEqual('%g' % 0.0, '0') &&
std.assertEqual('%#g' % 0.0, '0.00000') &&

// Negative values
std.assertEqual('%g' % -0.1, '-0.1') &&
std.assertEqual('%#g' % -0.1, '-0.100000') &&

// Very small exponents
std.assertEqual('%g' % 0.00001, '1e-05') &&
std.assertEqual('%#g' % 0.00001, '1.00000e-05') &&

// Precision 0 (treated as precision 1)
std.assertEqual('%.0g' % 0.0, '0') &&
std.assertEqual('%.0g' % 1.0, '1') &&
std.assertEqual('%.0g' % 0.1, '0.1') &&
std.assertEqual('%#.0g' % 1.0, '1.') &&

// Star width/precision follows Python-style integer semantics
std.assertEqual('%*g' % [10, 1.0], '         1') &&
std.assertEqual('%*g' % [10, 0.1], '       0.1') &&
std.assertEqual('%*g' % [4, 1.0], '   1') &&
std.assertEqual('%*g' % [-4, 1.0], '1   ') &&
std.assertEqual('%.*g' % [3, 1.23456], '1.23') &&
std.assertEqual('%.*g' % [-3, 1.23456], '1') &&
std.assertEqual('%*.*g' % [6, 3, 1.23456], '  1.23') &&
std.assertEqual('%*.*g' % [-6, 3, 1.23456], '1.23  ') &&

// Regression tests: values close to powers of 10
std.assertEqual('%g' % 99.9999, '99.9999') &&
std.assertEqual('%g' % 9.99999, '9.99999') &&
std.assertEqual('%g' % 999.999, '999.999') &&
std.assertEqual('%g' % 0.999999, '0.999999') &&
std.assertEqual('%.12g' % 9.99999999876, '9.99999999876') &&
std.assertEqual('%.12g' % 99.9999999876, '99.9999999876') &&
std.assertEqual('%.12g' % 0.999999999876, '0.999999999876') &&

true
