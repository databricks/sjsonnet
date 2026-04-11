// Verify that && type check is enforced inside function bodies (auto-TCO path).
// Without the fix, visitExprWithTailCallSupport would skip the rhs type check
// and silently return "hello" instead of erroring.
local f() = true && "hello";
f()
