// Explicit tailstrict inside `error value` should also be resolved by the
// outer trampoline instead of nested resolving inside the error expression.
local f(n) =
  if n <= 0 then "done"
  else error (f(n - 1) tailstrict);

f(10000)
