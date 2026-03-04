// Test hybrid materialization mode with deeply nested empty arrays.
// Depth 200 exceeds the default materializeRecursiveDepthLimit (128),
// so the first 128 levels use JVM stack recursion and the remaining
// levels fall back to the iterative ArrayDeque-based materializer.
local nest(depth) =
  local aux(acc, i) =
    if i <= 0 then acc
    else aux([acc], i - 1) tailstrict;
  aux([], depth);

nest(200)
