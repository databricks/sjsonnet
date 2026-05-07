// Nested boolean continuations should report the innermost operator that first
// observes the non-boolean TailCall result.
local f(n) =
  if n <= 0 then 0
  else true && (false || f(n - 1));

f(1000)
