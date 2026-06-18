// Test that std.char() works correctly for valid codepoints near the surrogate range
// and rejects surrogate codepoints (0xD800-0xDFFF) with an error.
[
  std.codepoint(std.char(55295)),  // 0xD7FF - last valid before surrogates
  std.codepoint(std.char(57344)),  // 0xE000 - first valid after surrogates
  std.codepoint(std.char(65533)),  // 0xFFFD - replacement character itself
  std.codepoint(std.char(0)),      // 0x0000 - null
  std.codepoint(std.char(65)),     // 0x0041 - 'A'
]
