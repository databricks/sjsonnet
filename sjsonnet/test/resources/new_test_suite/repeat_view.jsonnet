// Test cases for std.repeat with lazy repeated array view optimization
// Verifies that repeated arrays work correctly and maintain the same semantics
// as the original eager array copy implementation.

local tests = [
  // Basic repeat of simple array
  std.repeat([1, 2, 3], 3) == [1, 2, 3, 1, 2, 3, 1, 2, 3],
  
  // Edge case: repeat zero times
  std.repeat([1, 2, 3], 0) == [],
  
  // Edge case: repeat once
  std.repeat([1, 2, 3], 1) == [1, 2, 3],
  
  // Repeat empty array
  std.repeat([], 5) == [],
  
  // Repeat single element
  std.repeat([42], 5) == [42, 42, 42, 42, 42],
  
  // Repeat with mixed types
  std.repeat([1, "a", true], 2) == [1, "a", true, 1, "a", true],
  
  // Repeat nested array
  std.repeat([[1, 2], [3, 4]], 2) == [[1, 2], [3, 4], [1, 2], [3, 4]],
  
  // Access by index on repeated array  
  std.repeat([1, 2, 3], 4)[5] == 3,  // index 5 maps to position 5 % 3 = 2
  
  // Length of repeated array
  std.length(std.repeat([1, 2, 3], 1000)) == 3000,
  
  // Repeat string
  std.repeat("ab", 3) == "ababab",
  
  // Concat with repeated array
  std.repeat([1, 2], 2) + [3, 4] == [1, 2, 1, 2, 3, 4],
  
  // Sort repeated array
  std.sort(std.repeat([3, 1, 2], 2)) == [1, 1, 2, 2, 3, 3],
];

// Test all conditions pass
std.all(tests)
