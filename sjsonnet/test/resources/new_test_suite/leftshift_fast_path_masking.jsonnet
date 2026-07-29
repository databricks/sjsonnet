std.assertEqual((1 << 64) * 1, 1) &&
std.assertEqual(((1 << 64) + 0) * 1, 1) &&
std.assertEqual((1 << 65) * 1, 2) &&
std.assertEqual((1 << 128) * 1, 1) &&
std.assertEqual((0 << 128) * 1, 0) &&
true
