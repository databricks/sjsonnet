// Test automatic tail-call optimization (auto-TCO).
// The StaticOptimizer detects self-recursive calls in tail position and
// marks them as tailstrict, enabling the TailCall trampoline without
// requiring the user to write 'tailstrict' at each recursive call site.
//
// sjsonnet's default maxStack is 500, so any recursion deeper than 500
// would fail with "Max stack frames exceeded" without TCO. The depths
// used here (10000) clearly exceed that limit, proving auto-TCO is active.

// Direct function binding form: local sum(n, acc) = ...
local sum(n, acc) =
  if n == 0 then acc
  else sum(n - 1, acc + n);

// Function literal binding form: local counter = function(n, acc) ...
local counter = function(n, acc)
  if n == 0 then acc
  else counter(n - 1, acc + 1);

// Non-tail call: multiplication wraps the recursive call, so auto-TCO
// must NOT mark it (it would change semantics). Shallow depth is fine.
local factorial(n) =
  if n <= 1 then 1
  else n * factorial(n - 1);

// Multiple tail positions through if-else chains
local collatz_steps(n, steps) =
  if n == 1 then steps
  else if n % 2 == 0 then collatz_steps(n / 2, steps + 1)
  else collatz_steps(3 * n + 1, steps + 1);

// Tail call through local binding
local count_down(n) =
  local next = n - 1;
  if n == 0 then "done"
  else count_down(next);

// Depth 10000 >> maxStack(500): proves auto-TCO trampoline is active
std.assertEqual(sum(10000, 0), 50005000) &&
// Function-literal form also gets auto-TCO
std.assertEqual(counter(10000, 0), 10000) &&
// Non-tail recursion at safe depth, correctness check
std.assertEqual(factorial(10), 3628800) &&
// Multi-branch tail calls
std.assertEqual(collatz_steps(27, 0), 111) &&
// Tail call through local binding
std.assertEqual(count_down(10000), "done") &&

// Lazy semantics regression test: auto-TCO must NOT eagerly evaluate arguments.
// Without this fix, error("boom") would be eagerly evaluated and crash.
// With correct auto-TCO (TailstrictModeAutoTCO), args stay lazy and error("boom")
// is never evaluated because the function returns 1 on the second call.
local lazy_check(x, y) =
  if x then 1
  else lazy_check(true, error "boom");
std.assertEqual(lazy_check(false, 42), 1) &&

true
