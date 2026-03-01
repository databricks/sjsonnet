// Test hybrid materialization mode with deeply nested empty arrays.
// Depth 100 exceeds the default materializeRecursiveDepthLimit (64),
// so the first 64 levels use JVM stack recursion and the remaining
// levels fall back to the iterative ArrayDeque-based materializer.
local nest(depth) =
  local aux(acc, i) =
    if i <= 0 then acc
    else aux([acc], i - 1) tailstrict;
  aux([], depth);

nest(100)
