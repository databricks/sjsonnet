// Test +: merge with compatible types
assert ({a: "hello"} + {a+: " world"}).a == "hello world";
assert ({a: 1} + {a+: 2}).a == 3;
assert ({a: [1]} + {a+: [2]}).a == [1, 2];
true
