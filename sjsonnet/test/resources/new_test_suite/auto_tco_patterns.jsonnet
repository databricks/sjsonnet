// Comprehensive test for automatic tail-call optimization (auto-TCO).
// Tests ALL tail position patterns that the StaticOptimizer should detect.
//
// Depth 10000 >> maxStack(500): proves auto-TCO trampoline is active.

// 1. Direct function binding: local sum(n, acc) = ...
local sum(n, acc) =
  if n == 0 then acc
  else sum(n - 1, acc + n);

// 2. Function literal binding: local counter = function(n, acc) ...
local counter = function(n, acc)
  if n == 0 then acc
  else counter(n - 1, acc + 1);

// 3. Non-tail call: multiplication wraps the recursive call — auto-TCO must NOT mark it
local factorial(n) =
  if n <= 1 then 1
  else n * factorial(n - 1);

// 4. Multiple tail positions through if-else chains
local collatz_steps(n, steps) =
  if n == 1 then steps
  else if n % 2 == 0 then collatz_steps(n / 2, steps + 1)
  else collatz_steps(3 * n + 1, steps + 1);

// 5. Tail call through local binding
local count_down(n) =
  local next = n - 1;
  if n == 0 then "done"
  else count_down(next);

// 6. Tail call through || operator (rhs of || is tail position)
// Note: || requires boolean rhs, so we use a boolean-returning function
local f_or(n) = if n <= 0 then true else (false || f_or(n - 1));

// 7. Tail call through && operator (rhs of && is tail position)
local f_and(n) = if n <= 0 then true else (true && f_and(n - 1));

// 8. Nested || and && tail positions
local f_nested_or(n) = if n <= 0 then true else (false || (false || f_nested_or(n - 1)));
local f_nested_and(n) = if n <= 0 then true else (true && (true && f_nested_and(n - 1)));

// 9. Mixed || and && tail positions
local f_mixed(n) = if n <= 0 then true else (true || (false && f_mixed(n - 1)));

// 10. Tail call through assert (returned expr is tail position)
local f_assert(n) =
  assert n >= 0;
  if n == 0 then 0 else f_assert(n - 1);

// 11. Lazy semantics: auto-TCO must NOT eagerly evaluate arguments
local lazy_check(x, y) =
  if x then 1
  else lazy_check(true, error "boom");

// 12. Mutual recursion test (shallow depth — NOT auto-TCO'd, only direct self-recursion is)
// Using an object to make both functions visible to each other
local even_odd = {
  even(n):: if n == 0 then true else self.odd(n - 1),
  odd(n):: if n == 0 then false else self.even(n - 1),
};

// 13. Deeply nested local expressions
local f_nested_local(n) =
  local a = n - 1;
  local b = a;
  local c = b;
  if n <= 0 then 0 else f_nested_local(c);

// 14. Tail call in else branch of deeply nested if-else
local f_deep_if(n) =
  if n <= 0 then 0
  else
    if n == 1 then 1
    else
      if n == 2 then 2
      else f_deep_if(n - 1);

// 15. Function with 0 params (Apply0)
local f_zero() = 42;

// 16. Function with 4+ params (generic Apply)
local f_four(a, b, c, d) =
  if a <= 0 then d
  else f_four(a - 1, b, c, d + 1);

// 17. Edge case: &&/|| with base case in outer if, recursive call in &&/|| rhs.
// This tests that hasNonRecursiveExit correctly identifies the outer if's base case
// (not the &&/|| rhs which always recurses).
local f_and_with_outer_base(n) =
  if n <= 0 then 0  // outer base case
  else (true && f_and_with_outer_base(n - 1));  // rhs always recurses, but outer if provides exit

local f_or_with_outer_base(n) =
  if n <= 0 then 0
  else (false || f_or_with_outer_base(n - 1));

// Run tests
std.assertEqual(sum(10000, 0), 50005000) &&
std.assertEqual(counter(10000, 0), 10000) &&
std.assertEqual(factorial(10), 3628800) &&
std.assertEqual(collatz_steps(27, 0), 111) &&
std.assertEqual(count_down(10000), "done") &&
std.assertEqual(f_or(10000), true) &&
std.assertEqual(f_and(10000), true) &&
std.assertEqual(f_nested_or(10000), true) &&
std.assertEqual(f_nested_and(10000), true) &&
std.assertEqual(f_mixed(10000), true) &&
std.assertEqual(f_assert(10000), 0) &&
std.assertEqual(lazy_check(false, 42), 1) &&
// Mutual recursion (shallow depth — NOT auto-TCO'd)
std.assertEqual(even_odd.even(10), true) &&
std.assertEqual(even_odd.odd(10), false) &&
std.assertEqual(f_nested_local(10000), 0) &&
std.assertEqual(f_deep_if(10000), 2) &&
std.assertEqual(f_zero(), 42) &&
std.assertEqual(f_four(10000, 0, 0, 0), 10000) &&
// Edge case: &&/|| with outer base case — auto-TCO should work because outer if provides exit
std.assertEqual(f_and_with_outer_base(10000), 0) &&
std.assertEqual(f_or_with_outer_base(10000), 0) &&

true
