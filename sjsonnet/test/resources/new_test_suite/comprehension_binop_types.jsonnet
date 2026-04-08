// Regression test: all binary operators in comprehensions with ValidId operands
local strs = ["hello", "world"];
local nums = [1, 2, 3];
local arrs = [[1, 2], [3, 4]];

// String concatenation
local str_concat = [a + b for a in strs for b in strs];

// Numeric arithmetic
local num_add = [a + b for a in nums for b in nums];
local num_sub = [a - b for a in [10, 20] for b in [3, 5]];
local num_mul = [a * b for a in [2, 3] for b in [4, 5]];
local num_div = [a / b for a in [10, 20] for b in [2, 5]];
local num_mod = [a % b for a in [10, 7] for b in [3, 4]];

// Comparison operators
local cmp_lt = [a < b for a in nums for b in nums];
local cmp_eq = [a == b for a in nums for b in nums];
local cmp_ne = [a != b for a in nums for b in nums];

// Bitwise operators
local bw_and = [a & b for a in [3, 5] for b in [6, 7]];
local bw_or  = [a | b for a in [3, 5] for b in [6, 7]];
local bw_xor = [a ^ b for a in [3, 5] for b in [6, 7]];
local bw_shl = [a << b for a in [1, 2] for b in [1, 2]];
local bw_shr = [a >> b for a in [8, 16] for b in [1, 2]];

// String formatting
local str_fmt = [a % b for a in ["val=%d", "x=%d"] for b in [42, 99]];

// Array concatenation
local arr_concat = [a + b for a in arrs for b in arrs];

// 'in' operator
local objs = [{a: 1}, {b: 2}];
local in_test = [a in b for a in ["a", "b"] for b in objs];

std.assertEqual(str_concat, ["hellohello", "helloworld", "worldhello", "worldworld"]) &&
std.assertEqual(num_add, [2, 3, 4, 3, 4, 5, 4, 5, 6]) &&
std.assertEqual(num_sub, [7, 5, 17, 15]) &&
std.assertEqual(num_mul, [8, 10, 12, 15]) &&
std.assertEqual(num_div, [5, 2, 10, 4]) &&
std.assertEqual(num_mod, [1, 2, 1, 3]) &&
std.assertEqual(cmp_lt, [false, true, true, false, false, true, false, false, false]) &&
std.assertEqual(cmp_eq, [true, false, false, false, true, false, false, false, true]) &&
std.assertEqual(cmp_ne, [false, true, true, true, false, true, true, true, false]) &&
std.assertEqual(bw_and, [2, 3, 4, 5]) &&
std.assertEqual(bw_or, [7, 7, 7, 7]) &&
std.assertEqual(bw_xor, [5, 4, 3, 2]) &&
std.assertEqual(bw_shl, [2, 4, 4, 8]) &&
std.assertEqual(bw_shr, [4, 2, 8, 4]) &&
std.assertEqual(str_fmt, ["val=42", "val=99", "x=42", "x=99"]) &&
std.assertEqual(arr_concat, [[1, 2, 1, 2], [1, 2, 3, 4], [3, 4, 1, 2], [3, 4, 3, 4]]) &&
std.assertEqual(in_test, [true, false, false, true]) &&
true
