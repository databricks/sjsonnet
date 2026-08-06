// Lone surrogates in JSON strings are replaced with U+FFFD, matching
// go-jsonnet and sjsonnet's own std.char / %c surrogate policy. Keeping
// them would corrupt to '?' on UTF-8 output.
std.assertEqual(std.parseJson('"\\ud800"'), "\ufffd") &&
std.assertEqual(std.parseJson('"\\ud800\\ud800"'), "\ufffd\ufffd") &&
std.assertEqual(std.parseJson('"a\\udc00b"'), "a\ufffdb") &&
// a valid surrogate pair is preserved
std.assertEqual(std.parseJson('"\\ud83d\\ude00"'), "\ud83d\ude00") &&
// object keys are sanitized too
std.assertEqual(std.parseJson('{"\\ud800": 1}')['\ufffd'], 1)
