// Verify that -0 is preserved across all manifest functions (matching go-jsonnet).

local jsonOut = std.manifestJson({a: -0});
local jsonMinOut = std.manifestJsonMinified({a: -0});
local jsonExOut = std.manifestJsonEx({a: -0}, '  ');
local yamlOut = std.manifestYamlDoc({a: -0});
local tomlOut = std.manifestTomlEx({a: -0}, '  ');
local strConcat = (-0) + 'x';
local toStringOut = std.toString(-0);

std.assertEqual(jsonOut, '{\n    "a": -0\n}') &&
std.assertEqual(jsonMinOut, '{"a":-0}') &&
std.assertEqual(jsonExOut, '{\n  "a": -0\n}') &&
std.assertEqual(yamlOut, '"a": -0') &&
std.assertEqual(tomlOut, 'a = -0') &&
std.assertEqual(strConcat, '-0x') &&
std.assertEqual(toStringOut, '-0') &&
true
