// Test that comparing arrays containing function elements errors correctly.
// This was a bug where shared function references in arrays skipped the equality check.
local f = function(x) x;
[f, 1] == [f, 1]
