// Width padding must count codepoints, not UTF-16 code units.
// Supplementary characters (surrogate pairs) count as 1 codepoint.
[
  std.assertEqual("%5s" % "😀", "    😀"),
  std.assertEqual("%-5s" % "😀", "😀    "),
  std.assertEqual("%5s" % "a😀b", "  a😀b"),
  std.assertEqual("%3s" % "😀😀", " 😀😀"),
  std.assertEqual("%5.1s" % "😀x", "    😀"),
  std.assertEqual("%5s" % "hello", "hello"),
  std.assertEqual("%10s" % "hello", "     hello"),
]
