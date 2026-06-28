// Regression guard for #840 (fast-path strict json imports): imported JSON objects use an
// uncapped inline-array layout, so a large imported object that gains a `super` (via `+`)
// must resolve fields per-key during materialize. Pre-fix that was an O(N) linear scan per
// key (O(N^2) per object); the fix lazily builds an O(1) lookup map for large inline objects.
local data = import "json_import_merge_data.json";
[d + { extra: 1 } for d in data]
