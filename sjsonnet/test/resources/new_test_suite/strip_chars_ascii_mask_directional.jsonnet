std.assertEqual(std.stripChars("abcXYZcba", "abcXYZ"), "") &&
std.assertEqual(std.lstripChars("abcXYZkeepXYZ", "abcXYZ"), "keepXYZ") &&
std.assertEqual(std.rstripChars("XYZkeepabcXYZ", "abcXYZ"), "XYZkeep") &&
std.assertEqual(std.stripChars("éabcé", "abc"), "éabcé") &&
std.assertEqual(std.stripChars("😀abc😀", "abc"), "😀abc😀")
