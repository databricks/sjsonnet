// Regression guard for repeated sorted std.objectFieldsAll calls on the same large imported object.
// Unlike visible keys, all keys did not have a sorted cache, so this path used to clone and sort
// the same key array on every call.
local data = import "json_import_merge_data.json";
local d = data[0];
std.assertEqual(
  std.sum([std.length(std.objectFieldsAll(d)) for _ in std.range(1, 2000)]),
  2000000
)
