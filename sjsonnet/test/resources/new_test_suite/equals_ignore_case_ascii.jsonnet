// Directional std.equalsIgnoreCase tests: official Jsonnet uses asciiLower semantics.

std.assertEqual(std.equalsIgnoreCase('FOo', 'foO'), true) &&
std.assertEqual(std.equalsIgnoreCase('É', 'é'), false) &&
std.assertEqual(std.equalsIgnoreCase('İ', 'i'), false)
