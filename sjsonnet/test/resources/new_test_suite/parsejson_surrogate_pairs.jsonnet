local obj = std.parseJson('{"\\uD83D\\uDE00": "\\u0041"}');

std.assertEqual(std.codepoint(std.parseJson('"\\uD83D\\uDE00"')), 128512) &&
std.assertEqual(std.parseJson('"\\u0041"'), "A") &&
std.assertEqual(std.objectFields(obj), ["😀"]) &&
std.assertEqual(obj["😀"], "A")
