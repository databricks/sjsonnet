local shared = ["x", ["y"]];
local left = ["a"];
local right = [["b"], "c"];
local lazyConcatLeft = std.makeArray(256, function(i) []);
local lazyConcat = lazyConcatLeft + [["b"], "c"];
local nest(n, x) = if n == 0 then x else [nest(n - 1, x)];
{
  deepJoin: std.deepJoin([shared, shared]),
  deepJoinConcatView: std.deepJoin(left + right),
  deepJoinDeep: std.deepJoin(nest(200, "z")),
  deepJoinLazyConcatView: std.deepJoin(lazyConcat),
  flattenDeepArray: std.flattenDeepArray([shared, shared]),
  flattenConcatView: std.flattenDeepArray(left + right),
  flattenDeepArrayDeep: std.flattenDeepArray(nest(200, "z")),
  flattenLazyConcatView: std.flattenDeepArray(lazyConcat),
  flattenReverseView: std.flattenDeepArray(std.reverse([["r2"], ["r1"]])),
}
