std.assertEqual(std.member('abc', ''), false) &&
std.assertEqual(std.member('', ''), false) &&
std.assertEqual(std.member('', 'a'), false) &&
std.assertEqual(std.member('abc', 'bc'), true) &&
std.assertEqual(std.member([''], ''), true) &&
std.assertEqual(std.member([''], 'a'), false)
