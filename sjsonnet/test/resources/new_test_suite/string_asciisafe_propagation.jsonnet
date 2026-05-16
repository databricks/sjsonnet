// Directional coverage for ASCII-safety propagation across StringModule builtins.
// Verifies that asciiSafe flag is correctly forwarded / cleared so that ByteRenderer's
// fast path stays correct after each transformation.

local mj = std.manifestJson;

// std.char: ASCII codepoint => asciiSafe; non-ASCII codepoint => not asciiSafe.
std.assertEqual(mj(std.char(65)), '"A"') &&
std.assertEqual(mj(std.char(233)), '"é"') &&
// Control / quote / backslash codepoints route through escape path.
std.assertEqual(mj(std.char(34)), '"\\""') &&
std.assertEqual(mj(std.char(92)), '"\\\\"') &&
std.assertEqual(mj(std.char(10)), '"\\n"') &&

// std.asciiUpper / asciiLower preserve asciiness — ASCII input stays asciiSafe.
std.assertEqual(mj(std.asciiUpper("hello")), '"HELLO"') &&
std.assertEqual(mj(std.asciiLower("WORLD")), '"world"') &&
// Non-ASCII input still renders correctly (UTF-8 path).
std.assertEqual(mj(std.asciiUpper("héllo")), '"HéLLO"') &&
std.assertEqual(mj(std.asciiLower("WÖRLD")), '"wÖrld"') &&

// std.strReplace: result asciiSafe iff src and `to` are.
std.assertEqual(mj(std.strReplace("hello world", "world", "everyone")), '"hello everyone"') &&
// Replace into ASCII source with non-ASCII `to` — must NOT be marked asciiSafe.
std.assertEqual(mj(std.strReplace("hello world", "world", "wörld")), '"hello wörld"') &&
// Non-ASCII source — must NOT be marked asciiSafe.
std.assertEqual(mj(std.strReplace("héllo world", "world", "everyone")), '"héllo everyone"') &&

// std.lstripChars / rstripChars / stripChars preserve asciiness.
std.assertEqual(mj(std.lstripChars("   hello", " ")), '"hello"') &&
std.assertEqual(mj(std.rstripChars("hello   ", " ")), '"hello"') &&
std.assertEqual(mj(std.stripChars("  hello  ", " ")), '"hello"') &&
// Strip on non-ASCII content still renders correctly.
std.assertEqual(mj(std.stripChars("  héllo  ", " ")), '"héllo"') &&

// std.split / splitLimit / splitLimitR: verify each element renders correctly.
std.assertEqual(std.split("a,b,c", ","), ["a", "b", "c"]) &&
std.assertEqual(std.splitLimit("a,b,c,d", ",", 2), ["a", "b", "c,d"]) &&
std.assertEqual(std.splitLimitR("a,b,c,d", ",", 2), ["a,b", "c", "d"]) &&
// Splits of non-ASCII string still yield correct elements.
std.assertEqual(std.split("á,b,c", ","), ["á", "b", "c"]) &&
// Split with quote / backslash chars stays correct after split.
std.assertEqual(std.split("a\"b,c", ","), ["a\"b", "c"]) &&
// Each element's manifestJson representation
std.assertEqual(mj(std.split("a,b,c", ",")[0]), '"a"') &&
std.assertEqual(mj(std.split("á,b,c", ",")[0]), '"á"') &&
std.assertEqual(mj(std.split("a\"b,c", ",")[0]), '"a\\"b"') &&

true
