// Incompatible types (number + boolean) should produce descriptive error, not MatchError
({a: 1} + {a+: true}).a
