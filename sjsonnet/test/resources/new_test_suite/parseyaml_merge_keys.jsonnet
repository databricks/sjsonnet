// Test that std.parseYaml correctly handles YAML merge keys (<<).
// Merge keys should inline referenced mapping fields with lower priority
// than explicit keys, matching go-jsonnet and jrsonnet behavior.

// Basic single-alias merge
local basic = std.parseYaml(|||
  defaults: &def
    x: 1
    y: 2
  item:
    <<: *def
    z: 3
|||);

// Explicit keys override merge keys
local override = std.parseYaml(|||
  defaults: &def
    x: 1
    y: 2
  item:
    <<: *def
    x: 99
    z: 3
|||);

// Merge key after explicit key: explicit still wins
local mergeAfter = std.parseYaml(|||
  defaults: &def
    x: 1
    y: 2
    z: 99
  item:
    z: 3
    <<: *def
|||);

// Sequence of aliases: earlier alias wins on conflict
local multiMerge = std.parseYaml(|||
  a: &a
    x: 1
    y: 2
  b: &b
    x: 99
    z: 3
  item:
    <<: [*a, *b]
    w: 4
|||);

// Nested merge keys (merge referencing a mapping that also has merge keys)
local nested = std.parseYaml(|||
  base: &base
    a: 1
  mid: &mid
    <<: *base
    b: 2
  item:
    <<: *mid
    c: 3
|||);

std.assertEqual(basic, {
  defaults: {x: 1, y: 2},
  item: {x: 1, y: 2, z: 3},
}) &&
std.assertEqual(override, {
  defaults: {x: 1, y: 2},
  item: {x: 99, y: 2, z: 3},
}) &&
std.assertEqual(mergeAfter, {
  defaults: {x: 1, y: 2, z: 99},
  item: {x: 1, y: 2, z: 3},
}) &&
std.assertEqual(multiMerge, {
  a: {x: 1, y: 2},
  b: {x: 99, z: 3},
  item: {x: 1, y: 2, z: 3, w: 4},
}) &&
std.assertEqual(nested, {
  base: {a: 1},
  mid: {a: 1, b: 2},
  item: {a: 1, b: 2, c: 3},
}) &&
true
