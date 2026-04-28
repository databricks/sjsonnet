// Directional parse tests: official Jsonnet parses into number values, not Long-bounded integers.

std.assertEqual(std.parseInt('9223372036854775808'), 9223372036854777856) &&
std.assertEqual(std.parseHex('10000000000000000'), 18446744073709551616) &&
std.assertEqual(std.parseOctal('1000000000000000000000'), 9223372036854775808) &&
std.assertEqual(std.parseHex(':'), 10) &&
std.assertEqual(std.parseHex('?'), 15)
