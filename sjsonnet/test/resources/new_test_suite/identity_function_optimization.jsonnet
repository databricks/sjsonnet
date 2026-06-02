// Issue #815: regression guard for the identity-function fast paths.
//
// sjsonnet elides calls to functions that are (effectively) identity and short-circuits
// `keyF=id`/default in set/sort/uniq. These optimizations are aggressive (apply1 elision,
// static self-composition classification in StaticOptimizer, cached effective-identity state),
// so this test pins their observable behavior: correct values, correct REJECTION of lookalikes,
// and preserved laziness. Every result is cross-checked against official Jsonnet semantics.

// --- direct identity elision ---
std.assertEqual((function(x) x)(5), 5) &&
std.assertEqual(std.map(function(x) x, [1, 2, 3]), [1, 2, 3]) &&

// --- self-composition over an effectively-identity captured g (staticIdentityShape == 2) ---
std.assertEqual((local g = function(x) x; local f = function(x) g(g(x)); f(7)), 7) &&
std.assertEqual((local g = function(x) x; local f = function(x) g(g(g(x))); f(4)), 4) &&
// cached effective-identity state must stay correct across repeated calls
std.assertEqual((local g = function(x) x; local f = function(x) g(g(x)); [f(1), f(2), f(1)]), [1, 2, 1]) &&

// --- lookalikes that must NOT be treated as identity ---
// captured g is x+1, so g(g(x)) = x+2, not identity
std.assertEqual((local g = function(x) x + 1; local f = function(x) g(g(x)); f(7)), 9) &&
// body references a captured var, not the parameter
std.assertEqual((local y = 99; (function(x) y)(5)), 99) &&
// two-parameter function returning its first arg is not unary identity
std.assertEqual((local f = function(x, y) x; f(1, 2)), 1) &&

// --- keyF = identity / default short-circuits in set & sort operations ---
std.assertEqual(std.sort([3, 1, 2], keyF=function(x) x), [1, 2, 3]) &&
std.assertEqual(std.uniq([1, 1, 2, 3, 3], keyF=function(x) x), [1, 2, 3]) &&
std.assertEqual(std.set([3, 1, 2, 1], keyF=function(x) x), [1, 2, 3]) &&
std.assertEqual(std.setUnion([1, 3], [2, 3], keyF=function(x) x), [1, 2, 3]) &&
std.assertEqual(std.setInter([1, 2, 3], [2, 3, 4], keyF=function(x) x), [2, 3]) &&
std.assertEqual(std.setDiff([1, 2, 3], [2], keyF=function(x) x), [1, 3]) &&

// --- unary-minus param function fast path (function(x) -x) in sort ---
std.assertEqual(std.sort([3, 1, 2], keyF=function(x) -x), [3, 2, 1]) &&

// --- laziness must be preserved: identity map must not force unused error elements ---
std.assertEqual(std.length(std.map(function(x) x, [1, error 'boom', 3])), 3) &&
std.assertEqual((local id = function(x) x; std.foldl(function(a, b) a + b, std.map(id, [1, 2, 3, 4]), 0)), 10) &&

true
