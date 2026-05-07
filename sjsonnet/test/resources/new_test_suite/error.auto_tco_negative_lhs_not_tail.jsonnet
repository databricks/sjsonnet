// Recursive calls in the lhs of && are not in tail position. This should remain
// unoptimized and hit max-stack at deep recursion rather than being auto-TCO'd.
local f(n) =
  if n <= 0 then true
  else f(n - 1) && true;

f(10000)
