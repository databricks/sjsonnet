// Directional std.setMember tests: strings are accepted as set-like indexables.

std.assertEqual(std.setMember('b', 'abc'), true) &&
std.assertEqual(std.setMember('d', 'abc'), false)
