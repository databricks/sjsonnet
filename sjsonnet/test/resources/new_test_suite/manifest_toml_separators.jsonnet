// Directional tests for std.manifestTomlEx section separator correctness.
// Each expected value is verified against go-jsonnet output.
// When the top-level object has no simple KV pairs, the output starts with \n\n
// (matching go-jsonnet's strings.Join(resSections, "\n\n") with empty sentinel).

local indent = '  ';

// Test 1: Empty section followed by regular section
local t1 = std.assertEqual(
  std.manifestTomlEx({ a: {}, b: { x: 1 } }, indent),
  '\n\n[a]\n\n[b]\n  x = 1');

// Test 2: Section with only nested children
local t2 = std.assertEqual(
  std.manifestTomlEx({ a: { b: { c: 1 } } }, indent),
  '\n\n[a]\n\n\n  [a.b]\n    c = 1');

// Test 3: Simple KV followed by section
local t3 = std.assertEqual(
  std.manifestTomlEx({ a: 1, b: { c: 2 } }, indent),
  'a = 1\n\n[b]\n  c = 2');

// Test 4: Multi-element table array with simple KV before it
local t4 = std.assertEqual(
  std.manifestTomlEx({ x: 1, arr: [{ a: 1 }, { b: 2 }] }, indent),
  'x = 1\n\n[[arr]]\n  a = 1\n\n[[arr]]\n  b = 2');

// Test 5: Multi-element table array without preceding simple KV
local t5 = std.assertEqual(
  std.manifestTomlEx({ arr: [{ a: 1 }, { b: 2 }] }, indent),
  '\n\n[[arr]]\n  a = 1\n\n[[arr]]\n  b = 2');

// Test 6: Empty table array element
local t6 = std.assertEqual(
  std.manifestTomlEx({ arr: [{}] }, indent),
  '\n\n[[arr]]');

// Test 7: Empty section followed by table array
local t7 = std.assertEqual(
  std.manifestTomlEx({ a: {}, arr: [{ x: 1 }] }, indent),
  '\n\n[a]\n\n[[arr]]\n  x = 1');

// Test 8: Table array followed by section with simple KV (alphabetical: arr < sec)
local t8 = std.assertEqual(
  std.manifestTomlEx({ sec: { k: 1 }, arr: [{ v: 2 }] }, indent),
  '\n\n[[arr]]\n  v = 2\n\n[sec]\n  k = 1');

// Test 9: Nested table array (table array inside a section)
local t9 = std.assertEqual(
  std.manifestTomlEx({ sec: { items: [{ n: 'a' }, { n: 'b' }] } }, indent),
  '\n\n[sec]\n\n\n  [[sec.items]]\n    n = "a"\n\n  [[sec.items]]\n    n = "b"');

// Test 10: Table array element with both simple KV and nested section
local t10 = std.assertEqual(
  std.manifestTomlEx({ arr: [{ x: 1, nested: { y: 2 } }] }, indent),
  '\n\n[[arr]]\n  x = 1\n\n  [arr.nested]\n    y = 2');

// Test 11: Multiple sections in sequence (all with content)
local t11 = std.assertEqual(
  std.manifestTomlEx({ a: { x: 1 }, b: { y: 2 }, c: { z: 3 } }, indent),
  '\n\n[a]\n  x = 1\n\n[b]\n  y = 2\n\n[c]\n  z = 3');

// Test 12: Table array followed by regular section
local t12 = std.assertEqual(
  std.manifestTomlEx({ arr: [{ a: 1 }], sec: { b: 2 } }, indent),
  '\n\n[[arr]]\n  a = 1\n\n[sec]\n  b = 2');

t1 && t2 && t3 && t4 && t5 && t6 && t7 && t8 && t9 && t10 && t11 && t12
