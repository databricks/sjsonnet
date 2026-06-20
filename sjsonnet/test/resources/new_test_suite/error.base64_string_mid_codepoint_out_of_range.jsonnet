// std.base64 must reject string inputs with codepoints outside [0, 255]. This
// case uses a BMP CJK character (世 = U+4E16 = codepoint 19990) embedded in an
// otherwise-ASCII string to exercise the "mid-string" failure path. The error
// message must include the offending codepoint value.
std.base64("hello" + std.char(19990))
