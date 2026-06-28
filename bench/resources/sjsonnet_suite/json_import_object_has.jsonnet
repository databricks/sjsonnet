// Regression guard for large imported JSON objects queried repeatedly via objectHas/objectHasAll.
// Large imported objects may use uncapped inline arrays, so repeated membership checks must use
// the cached lookup map above the inline scan threshold instead of doing O(N) scans per key.
local data = import "json_import_merge_data.json";
std.assertEqual(
  [
    std.length([k for k in std.objectFields(d) if std.objectHas(d, k)]) +
    std.length([k for k in std.objectFieldsAll(d) if std.objectHasAll(d, k)]) +
    (if std.objectHas(d, "missing") || std.objectHasAll(d, "missing") then 1 else 0)
    for d in data
  ],
  [2000 for d in data]
)
