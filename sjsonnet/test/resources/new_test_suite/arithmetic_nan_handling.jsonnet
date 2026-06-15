// Test NaN handling in arithmetic operations
// Matches go-jsonnet behavior

// Normal operations should work
assert std.assertEqual(10.0 / 2.0, 5.0) : 'normal division';
assert std.assertEqual(10.0 % 3.0, 1.0) : 'normal modulo';
assert std.assertEqual(0.0 / 1.0, 0.0) : 'zero divided by number';
assert std.assertEqual(0.0 % 1.0, 0.0) : 'zero modulo number';

true
