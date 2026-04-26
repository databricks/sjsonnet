// Directional std.asciiUpper/std.asciiLower tests: only ASCII letters are converted.

std.assertEqual(std.asciiUpper('azAZéßı'), 'AZAZéßı') &&
std.assertEqual(std.asciiLower('azAZÉİ'), 'azazÉİ')
