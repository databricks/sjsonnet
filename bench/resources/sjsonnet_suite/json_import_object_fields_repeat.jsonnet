// Regression guard for repeated sorted std.objectFields calls on the same large imported object.
// The object caches visible key names, so default sorted reflection should also reuse the sorted
// key cache instead of cloning and sorting on every call.
local data = import "json_import_merge_data.json";
local d = data[0];
std.assertEqual(
  std.sum([std.length(std.objectFields(d)) for _ in std.range(1, 2000)]),
  2000000
)
