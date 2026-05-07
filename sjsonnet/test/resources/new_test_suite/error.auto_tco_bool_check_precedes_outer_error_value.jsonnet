// The error message expression is evaluated before the outer error throws. If
// that expression itself fails a delayed boolean check, the boolean error wins.
local f(n) =
  if n <= 0 then 0
  else true && f(n - 1);

error f(1000)
