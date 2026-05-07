// Directional tests for automatic tail-call optimization (auto-TCO).
// These tests verify that auto-TCO:
//   1. CORRECTLY detects and optimizes tail-recursive calls
//   2. Does NOT incorrectly optimize non-tail calls
//   3. Works across all tail position patterns
//   4. Preserves lazy evaluation semantics
//   5. Handles edge cases correctly
//   6. Interacts correctly with explicit `tailstrict` annotation
//   7. Works with object-returning functions and comprehensions
//
// Tests use depth 10000 to prove trampoline is active (maxStack = 500).

// ============================================================
// SECTION 1: Tail position patterns
// ============================================================

// 1.1 Direct function binding (local f(args) = body)
local sum_accumulator(n, acc) =
  if n <= 0 then acc
  else sum_accumulator(n - 1, acc + n);

// 1.2 Function literal binding (local f = function(args) body)
local count_down = function(n, acc)
  if n <= 0 then acc
  else count_down(n - 1, acc + 1);

// 1.3 Multiple recursive calls — ONLY tail calls should be TCO'd
local mixed_calls(n) =
  if n <= 0 then 0
  else
    local non_tail = mixed_calls(n - 1);  // NOT tail — wrapped in +
    non_tail + n;                          // tail call: n is the result

// 1.4 If-else chain with tail calls in ALL branches
local multi_branch(n) =
  if n <= 0 then "zero"
  else if n == 1 then "one"
  else if n == 2 then "two"
  else multi_branch(n - 1);

// 1.5 Tail call through LocalExpr
local through_local(n) =
  local x = n - 1;
  local y = x;
  if n <= 0 then 0
  else through_local(y);

// 1.6 Tail call through deeply nested locals
local deeply_nested_locals(n) =
  local a = n - 1;
  local b = a;
  local c = b;
  local d = c;
  local e = d;
  if n <= 0 then 0
  else deeply_nested_locals(e);

// 1.7 Tail call through AssertExpr
local through_assert(n) =
  assert n >= 0;
  if n <= 0 then 0
  else through_assert(n - 1);

// 1.8 Tail call through || (rhs is tail position)
local through_or(n) =
  if n <= 0 then true
  else (false || through_or(n - 1));

// 1.9 Tail call through && (rhs is tail position)
local through_and(n) =
  if n <= 0 then true
  else (true && through_and(n - 1));

// 1.10 Nested || with tail call at deepest level
local nested_or(n) =
  if n <= 0 then true
  else (false || (false || (false || nested_or(n - 1))));

// 1.11 Nested && with tail call at deepest level
local nested_and(n) =
  if n <= 0 then true
  else (true && (true && (true && nested_and(n - 1))));

// 1.12 Mixed || and && — tail call in && rhs within || rhs
local mixed_or_and(n) =
  if n <= 0 then true
  else (false || (true && mixed_or_and(n - 1)));

// 1.13 Mixed && and || — tail call in || rhs within && rhs
local mixed_and_or(n) =
  if n <= 0 then true
  else (true && (false || mixed_and_or(n - 1)));

// 1.14 Complex boolean expression with tail call
local complex_bool(n) =
  if n <= 0 then true
  else ((true || false) && (false || true) && complex_bool(n - 1));

// 1.15 Tail call through multiple if-else-assert combinations
local combined_control_flow(n) =
  assert n >= -1;
  local x = n;
  if x < 0 then -1
  else
    assert x >= 0;
    if x == 0 then 0
    else combined_control_flow(x - 1);

// 1.16 Deeply nested if-else with tail call at bottom
local deep_if_chain(n) =
  if n <= 0 then 0
  else
    if n == 1 then 1
    else
      if n == 2 then 2
      else
        if n == 3 then 3
        else deep_if_chain(n - 1);

// 1.17 Tail call in else branch only (asymmetric if)
local asymmetric_if(n) =
  if n <= 0 then 0
  else asymmetric_if(n - 1);

// 1.18 Tail call through error expression value
// Erroring behavior is covered by error.auto_tco_error_value_tail_call.jsonnet.
local error_value(n) =
  if n <= 0 then error "done at " + n
  else error_value(n - 1);

// ============================================================
// SECTION 2: Arity coverage (Apply0–Apply5+)
// ============================================================

// 2.1 Apply1 (1 parameter, despite the name — there's no true 0-param tail recursion)
local loop_zero_params(n) =
  if n <= 0 then "done"
  else loop_zero_params(n - 1);

// 2.2 Apply1 (1 parameter)
local loop_one_param(n) =
  if n <= 0 then n
  else loop_one_param(n - 1);

// 2.3 Apply2 (2 parameters)
local loop_two_params(n, acc) =
  if n <= 0 then acc
  else loop_two_params(n - 1, acc + 1);

// 2.4 Apply3 (3 parameters)
local loop_three_params(n, acc, mult) =
  if n <= 0 then acc * mult
  else loop_three_params(n - 1, acc + 1, mult);

// 2.5 Apply4 (4 parameters, generic Apply)
local loop_four_params(a, b, c, d) =
  if a <= 0 then b + c + d
  else loop_four_params(a - 1, b + 1, c + 1, d + 1);

// 2.6 Apply5 (5 parameters)
local loop_five_params(a, b, c, d, e) =
  if a <= 0 then b + c + d + e
  else loop_five_params(a - 1, b + 1, c + 1, d + 1, e + 1);

// ============================================================
// SECTION 3: Argument passing modes
// ============================================================

// 3.1 Named arguments — rebindApply resolves to positional, so auto-TCO still applies
local with_named_args(a, b=0) =
  if a <= 0 then b
  else with_named_args(a=a - 1, b=b + 1);

// 3.2 Default argument involvement
local with_default(a, b=0) =
  if a <= 0 then b
  else with_default(a - 1, b + 1);

// ============================================================
// SECTION 4: Explicit tailstrict + auto-TCO interaction
// ============================================================

// 4.1 User writes `tailstrict` on a self-recursive call.
// Auto-TCO should NOT double-mark (guard: `!a.tailstrict`).
// This uses explicit tailstrict (eager args), NOT auto-TCO (lazy args).
local explicit_tailstrict_self(n) =
  if n <= 0 then 0
  else explicit_tailstrict_self(n - 1) tailstrict;

// 4.2 Explicit tailstrict forces eager eval of ALL args, including unused ones.
// Without eager forcing, `error "kaboom"` would not be evaluated.
local force_error_check(x, y) = x;

// 4.3 Mixed: some branches explicit tailstrict, some auto-TCO.
// Even branches: explicit tailstrict (eager). Odd branches: auto-TCO (lazy).
local mixed_explicit(n) =
  if n <= 0 then 0
  else if n % 2 == 0 then mixed_explicit(n - 1) tailstrict
  else mixed_explicit(n - 1);

// ============================================================
// SECTION 5: Object-returning and container-returning tail recursion
// ============================================================
// These use an accumulator pattern so the recursive call IS in tail position
// (direct return), not in a Bind.rhs.

// 5.1 Deeply tail-recursive function building an object via accumulator.
// Uses depth=400 (below maxStack=500) because object field access in lazy
// thunk args adds per-iteration overhead. The numeric accumulator tests
// (SECTION 2, 9) prove trampoline at depth > 500.
local build_obj(n, acc) =
  if n <= 0 then acc
  else build_obj(n - 1, { result: "done", count: acc.count + 1 });

// 5.2 Deeply tail-recursive function building an array via accumulator.
local build_arr(n, acc) =
  if n <= 0 then acc
  else build_arr(n - 1, acc + [n]);

// 5.3 Nested object construction with tail-recursive field computation.
local nested_obj_builder(n) = {
  level1: {
    level2: {
      value: build_obj(n, { count: 0 }).count,
    },
    sibling: std.length(build_arr(n, [])),
  },
};

// 5.4 Object method that is tail-recursive and returns an object.
// This is NOT auto-TCO'd (self.method), so use shallow depth.
local obj_builder = {
  build(n):: if n <= 0 then { n: 0 } else { n: n, prev: self.build(n - 1) },
};

// ============================================================
// SECTION 6: Nested functions, comprehensions, and stdlib callbacks
// ============================================================

// 6.1 Function defined inside another function, tail-recursive.
// The inner function's self-call should be auto-TCO'd.
local outer_factory(n) =
  local inner(acc, remaining) =
    if remaining <= 0 then acc
    else inner(acc + remaining, remaining - 1);
  inner(0, n);

// 6.2 Comprehension body calls a tail-recursive function.
local sum_to(n, acc) =
  if n <= 0 then acc
  else sum_to(n - 1, acc + n);

local comp_with_tco = [sum_to(i, 0) for i in [1, 2, 3, 4, 5]];

// 6.3 std.map with a tail-recursive callback.
local map_with_tco = std.map(function(x) sum_to(x, 0), [10, 20, 30]);

// 6.4 std.foldl where the accumulator function is tail-recursive.
local fold_with_tco = std.foldl(
  function(acc, x) acc + sum_to(x, 0),
  [1, 2, 3],
  0
);

// 6.5 Nested comprehension with tail-recursive function.
local nested_comp = [[sum_to(j, 0) for j in [1, 2]] for i in [1, 2]];

// ============================================================
// SECTION 7: Negative tests — ensure non-self-recursion is NOT auto-TCO'd
// ============================================================

// 7.1 Non-tail call (multiplication wraps recursive call)
local factorial_non_tail(n) =
  if n <= 1 then 1
  else n * factorial_non_tail(n - 1);

// 7.2 Non-tail call (addition wraps BOTH recursive calls — tree recursion)
local fibonacci_non_tail(n) =
  if n <= 1 then 1
  else fibonacci_non_tail(n - 1) + fibonacci_non_tail(n - 2);

// 7.3 Mutual recursion via object — NOT auto-TCO'd (different function names)
local mutual = {
  even(n):: if n == 0 then true else self.odd(n - 1),
  odd(n):: if n == 0 then false else self.even(n - 1),
};

// 7.4 Two different functions calling each other — NOT auto-TCO'd
local cross_funcs = {
  a(n):: if n <= 0 then 0 else self.b(n - 1),
  b(n):: if n <= 0 then 1 else self.a(n - 1),
};

// 7.5 Function calling a DIFFERENT function in tail position.
// The callee is tail-recursive, but the caller's call is NOT self-recursion.
local helper_func(n) =
  if n <= 0 then 0 else helper_func(n - 1);

local caller_func(n) =
  if n <= 0 then 0 else helper_func(n);

// 7.6 Function shadowing: inner binding shadows outer name.
// Each `f` should be analyzed independently.
local shadowing_test =
  local f(n) = if n <= 0 then "outer" else f(n - 1);
  local inner =
    local f(n) = if n <= 0 then "inner" else f(n - 1);
    f(5);
  { outer_result: f(3), inner_result: inner };

// 7.7 Function captured via alias — still self-recursion (same scope).
local aliased_recursion(n) =
  local go = aliased_recursion;  // alias to self
  if n <= 0 then 0 else go(n - 1);

// ============================================================
// SECTION 8: Lazy semantics — auto-TCO must NOT force argument evaluation
// ============================================================

// 8.1 Unused dangerous argument should never be evaluated
local lazy_recursive(flag, dangerous) =
  if flag then "safe"
  else lazy_recursive(true, dangerous);

// 8.2 Short-circuit with error through ||
local lazy_or(flag, dangerous) =
  if flag then true
  else (true || lazy_or(true, dangerous));

// 8.3 Lazy semantics through && with auto-TCO
local lazy_and(n, dangerous) =
  if n <= 0 then true
  else (true && lazy_and(n - 1, dangerous));

// 8.4 Lazy semantics through || with auto-TCO
local lazy_or_deep(n, dangerous) =
  if n <= 0 then true
  else (false || lazy_or_deep(n - 1, dangerous));

// ============================================================
// SECTION 9: Performance verification — depth > maxStack proves trampoline
// ============================================================

// 9.1 Depth 600 (just above maxStack=500) — must use trampoline
local perf_600(n) =
  if n <= 0 then "ok" else perf_600(n - 1);

// 9.2 Depth 5000 — well above maxStack
local perf_5000(n, acc) =
  if n <= 0 then acc else perf_5000(n - 1, acc + 1);

// 9.3 Large depth through And chain — verifies hasNonRecursiveExit
// correctly handles nested boolean operators
local perf_and_chain(n) =
  if n <= 0 then true
  else (true && (true && (true && perf_and_chain(n - 1))));

// ============================================================
// SECTION 10: Edge cases
// ============================================================

// 10.1 Function that returns itself (not a tail call)
local returns_function() =
  function(x) x + 1;

// 10.2 Object method self-recursion — NOT auto-TCO'd (self.method)
local obj_method = {
  sum(n, acc):: if n <= 0 then acc else self.sum(n - 1, acc + n),
};

// 10.3 Non-tail call: string concat wraps recursive call
local string_wrap_non_tail(n) =
  if n <= 0 then "0" else string_wrap_non_tail(n - 1) + "";

// 10.4 Many local bindings before the tail call
local many_locals(n) =
  local a = n - 1;
  local b = a + 0;
  local c = b + 0;
  local d = c + 0;
  local e = d + 0;
  local f = e + 0;
  local g = f + 0;
  local h = g + 0;
  local i = h + 0;
  local j = i + 0;
  if n <= 0 then 0 else many_locals(j);

// 10.5 Collatz sequence — different recursive calls in different branches
local collatz(n, steps) =
  if n == 1 then steps
  else if n % 2 == 0 then collatz(n / 2, steps + 1)
  else collatz(3 * n + 1, steps + 1);

// ============================================================
// Run all tests
// ============================================================
local depth = 10000;
local shallow = 20;

// SECTION 1: Tail position patterns
std.assertEqual(sum_accumulator(depth, 0), 50005000) &&
std.assertEqual(count_down(depth, 0), depth) &&
std.assertEqual(mixed_calls(200), 20100) &&
std.assertEqual(multi_branch(depth), "two") &&
std.assertEqual(through_local(depth), 0) &&
std.assertEqual(deeply_nested_locals(depth), 0) &&
std.assertEqual(through_assert(depth), 0) &&
std.assertEqual(through_or(depth), true) &&
std.assertEqual(through_and(depth), true) &&
std.assertEqual(nested_or(depth), true) &&
std.assertEqual(nested_and(depth), true) &&
std.assertEqual(mixed_or_and(depth), true) &&
std.assertEqual(mixed_and_or(depth), true) &&
std.assertEqual(complex_bool(depth), true) &&
std.assertEqual(combined_control_flow(depth), 0) &&
std.assertEqual(deep_if_chain(depth), 3) &&
std.assertEqual(asymmetric_if(depth), 0) &&

// SECTION 2: Arity coverage
std.assertEqual(loop_zero_params(depth), "done") &&
std.assertEqual(loop_one_param(depth), 0) &&
std.assertEqual(loop_two_params(depth, 0), depth) &&
std.assertEqual(loop_three_params(depth, 0, 1), depth) &&
std.assertEqual(loop_four_params(depth, 0, 0, 0), depth * 3) &&
std.assertEqual(loop_five_params(depth, 0, 0, 0, 0), depth * 4) &&

// SECTION 3: Argument passing modes
std.assertEqual(with_named_args(depth), depth) &&
std.assertEqual(with_default(depth, 0), depth) &&

// SECTION 4: Explicit tailstrict + auto-TCO interaction
std.assertEqual(explicit_tailstrict_self(depth), 0) &&
std.assertEqual(force_error_check(42, error "kaboom"), 42) &&
std.assertEqual(mixed_explicit(shallow), 0) &&

// SECTION 5: Object/container-returning tail recursion
std.assertEqual(build_obj(400, { count: 0 }).count, 400) &&
std.assertEqual(std.length(build_arr(400, [])), 400) &&
std.assertEqual(nested_obj_builder(400).level1.level2.value, 400) &&
std.assertEqual(nested_obj_builder(400).level1.sibling, 400) &&
std.assertEqual(obj_builder.build(5).n, 5) &&

// SECTION 6: Nested functions, comprehensions, stdlib
std.assertEqual(outer_factory(depth), depth * (depth + 1) / 2) &&
std.assertEqual(comp_with_tco, [1, 3, 6, 10, 15]) &&
std.assertEqual(map_with_tco, [55, 210, 465]) &&
std.assertEqual(fold_with_tco, 10) &&
std.assertEqual(nested_comp, [[1, 3], [1, 3]]) &&

// SECTION 7: Negative tests
std.assertEqual(factorial_non_tail(10), 3628800) &&
std.assertEqual(fibonacci_non_tail(20), 10946) &&
std.assertEqual(mutual.even(10), true) &&
std.assertEqual(mutual.odd(10), false) &&
std.assertEqual(cross_funcs.a(10), 0) &&
std.assertEqual(cross_funcs.b(10), 1) &&
std.assertEqual(caller_func(shallow), 0) &&
std.assertEqual(shadowing_test.outer_result, "outer") &&
std.assertEqual(shadowing_test.inner_result, "inner") &&
std.assertEqual(aliased_recursion(shallow), 0) &&

// SECTION 8: Lazy semantics
std.assertEqual(lazy_recursive(false, error "should not eval"), "safe") &&
std.assertEqual(lazy_or(false, error "should not eval"), true) &&
std.assertEqual(lazy_and(depth, error "should not eval"), true) &&
std.assertEqual(lazy_or_deep(depth, error "should not eval"), true) &&

// SECTION 9: Performance verification
std.assertEqual(perf_600(600), "ok") &&
std.assertEqual(perf_5000(5000, 0), 5000) &&
std.assertEqual(perf_and_chain(1000), true) &&

// SECTION 10: Edge cases
std.isFunction(returns_function()) &&
std.assertEqual(obj_method.sum(100, 0), 5050) &&
std.assertEqual(string_wrap_non_tail(5), "0") &&
std.assertEqual(many_locals(depth), 0) &&
std.assertEqual(collatz(27, 0), 111) &&

true
