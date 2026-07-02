local nest(n, x) = if n == 0 then x else [nest(n - 1, x)];
local sharedStrings = std.makeArray(200, function(i) "x");
local sharedValues = std.makeArray(200, function(i) if i % 2 == 0 then "x" else i);
local sharedStringView = [sharedStrings + [], [] + sharedStrings, sharedStrings];
local sharedValueView = [sharedValues + [], [] + sharedValues, sharedValues];
{
  deepJoinFlat: std.length(std.deepJoin(std.makeArray(20000, function(i) "x"))),
  deepJoinNested: std.length(std.deepJoin(std.makeArray(1000, function(i) nest(20, "x")))),
  flattenFlat: std.length(std.flattenDeepArray(std.makeArray(20000, function(i) i))),
  flattenNested: std.length(std.flattenDeepArray(std.makeArray(1000, function(i) nest(20, i)))),
  sharedViewDeepJoin: std.length(std.deepJoin(sharedStringView)),
  sharedViewFlatten: std.length(std.flattenDeepArray(sharedValueView)),
}
