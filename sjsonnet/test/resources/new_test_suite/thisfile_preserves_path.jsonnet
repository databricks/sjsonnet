// Test that std.thisFile returns a proper path representation.
// When run from the test harness with a relative path, thisFile should
// contain the filename. When invoked via CLI with an absolute path,
// thisFile should preserve the absolute path (matching C++ jsonnet/go-jsonnet).
local thisFile = std.thisFile;
std.assertEqual(std.length(thisFile) > 0, true) &&
std.assertEqual(std.endsWith(thisFile, "thisfile_preserves_path.jsonnet"), true)
