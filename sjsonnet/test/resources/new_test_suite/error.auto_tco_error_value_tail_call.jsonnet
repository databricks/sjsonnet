// The value of `error value` is the last evaluated expression before throwing.
// A self-tail call there should still use the auto-TCO trampoline and preserve
// lazy arguments.
local f(n, unused) =
  if n <= 0 then "done"
  else error f(n - 1, unused);

f(10000, error "unused must stay lazy")
