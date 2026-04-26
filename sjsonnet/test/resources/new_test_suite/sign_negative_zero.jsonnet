// Directional std.sign test: -0 must return +0, matching official Jsonnet conditionals.

std.assertEqual(std.atan2(std.sign(-0), -1), std.atan2(0, -1))
