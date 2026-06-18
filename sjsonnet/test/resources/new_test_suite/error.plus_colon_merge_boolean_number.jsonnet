// Incompatible types (boolean + number) should produce descriptive error, not MatchError
({a: true} + {a+: 1}).a
