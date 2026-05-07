// Inner `error value` throws before the surrounding boolean operator can check
// the rhs result type.
local f(n) =
  if n <= 0 then 0
  else error f(n - 1);

true && f(1000)
