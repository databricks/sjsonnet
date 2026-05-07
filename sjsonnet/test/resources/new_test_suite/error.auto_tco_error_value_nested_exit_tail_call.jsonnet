// The non-recursive exit can be inside the `error value` expression. This should
// still be auto-TCO'd, because the value expression eventually produces the
// message that the surrounding error throws.
local f(n) =
  error if n <= 0 then "done" else f(n - 1);

f(10000)
