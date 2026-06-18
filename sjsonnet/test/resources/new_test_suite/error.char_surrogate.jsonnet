// std.char() should reject surrogate codepoints (0xD800-0xDFFF)
// High surrogate 0xD800 = 55296
std.char(55296)
