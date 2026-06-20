// std.base64 must reject string inputs whose codepoints fall outside [0, 255].
// The Jsonnet spec requires base64 input to be a byte string; go-jsonnet applies
// the same check and errors with "base64 encountered invalid codepoint value in
// the array (must be 0 <= X <= 255), got <value>". sjsonnet previously passed
// such strings through PlatformBase64 which silently UTF-8-encoded them,
// producing values that did not round-trip through std.base64Decode on the
// other implementations.

// Codepoint 256 (just past the 1-byte range) — should error.
std.base64(std.char(256))
