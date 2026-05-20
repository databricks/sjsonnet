// Directional coverage for Format ASCII-safety propagation.
// Ensures format strings preserve correct values across paths that produce Val.AsciiSafeStr:
// - simple %(name)s fast path with ASCII / non-ASCII literals and values
// - general format path with %s / %d / %c / %o / %x conversions
// - mixed ASCII literals + non-ASCII string interpolations (output must be correct)

std.assertEqual("hello %s" % "world", "hello world") &&
std.assertEqual("héllo %s" % "world", "héllo world") &&
std.assertEqual("hello %s" % "wörld", "hello wörld") &&
std.assertEqual("héllo %s" % "wörld", "héllo wörld") &&

// Simple %(name)s fast path: ASCII format + ASCII value
std.assertEqual("name=%(n)s" % { n: "alice" }, "name=alice") &&
// Simple %(name)s fast path: non-ASCII literal
std.assertEqual("námé=%(n)s" % { n: "alice" }, "námé=alice") &&
// Simple %(name)s fast path: non-ASCII value
std.assertEqual("name=%(n)s" % { n: "álice" }, "name=álice") &&
// Simple %(name)s fast path: repeated key with non-ASCII
std.assertEqual("%(n)s/%(n)s" % { n: "ümlaut" }, "ümlaut/ümlaut") &&

// Numeric conversions are always ASCII
std.assertEqual("%d" % 42, "42") &&
std.assertEqual("%05d" % 7, "00007") &&
std.assertEqual("%x" % 255, "ff") &&
std.assertEqual("%o" % 8, "10") &&
std.assertEqual("%.2f" % 3.14159, "3.14") &&

// %c with ASCII codepoint -> ASCII output
std.assertEqual("%c" % 65, "A") &&
// %c with non-ASCII codepoint -> non-ASCII output (must still render correctly)
std.assertEqual("%c" % 233, "é") &&

// Boolean / null
std.assertEqual("%s" % true, "true") &&
std.assertEqual("%s" % null, "null") &&

// Multi-spec
std.assertEqual("%s = %d, %s" % ["x", 1, "y"], "x = 1, y") &&

// Verify output is JSON-renderable correctly (this hits ByteRenderer's asciiSafe path)
std.assertEqual(std.manifestJson("héllo %s" % "wörld"), '"héllo wörld"') &&
std.assertEqual(std.manifestJson("hello %s" % "world"), '"hello world"') &&

true
