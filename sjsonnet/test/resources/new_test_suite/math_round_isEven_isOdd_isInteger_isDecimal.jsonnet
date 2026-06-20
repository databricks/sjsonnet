// === std.round: ties away from zero (go-jsonnet + jrsonnet agree) ===
assert std.round(0.5) == 1 : 'round(0.5) should be 1';
assert std.round(1.5) == 2 : 'round(1.5) should be 2';
assert std.round(2.5) == 3 : 'round(2.5) should be 3';
assert std.round(-0.5) == -1 : 'round(-0.5) should be -1 (away from zero)';
assert std.round(-1.5) == -2 : 'round(-1.5) should be -2 (away from zero)';
assert std.round(-2.5) == -3 : 'round(-2.5) should be -3 (away from zero)';
assert std.round(1.2) == 1 : 'round(1.2)';
assert std.round(1.8) == 2 : 'round(1.8)';
assert std.round(-1.2) == -1 : 'round(-1.2)';
assert std.round(-1.8) == -2 : 'round(-1.8)';
assert std.round(0) == 0 : 'round(0)';
assert std.round(1) == 1 : 'round(1)';
assert std.round(-1) == -1 : 'round(-1)';
// Large-integer regression: for |x| >= 2^52 the double is already an exact
// integer (ULP >= 1.0), so std.round must be the identity. go-jsonnet and
// jrsonnet agree on all four values below.
assert std.round(9007199254740991) == 9007199254740991 : 'round(2^53 - 1)';
assert std.round(9007199254740990) == 9007199254740990 : 'round(2^53 - 2)';
assert std.round(4503599627370497) == 4503599627370497 : 'round(2^52 + 1)';
assert std.round(-9007199254740991) == -9007199254740991 : 'round(-(2^53 - 1))';
assert std.round(1e20) == 1e20 : 'round(1e20)';

// === std.isEven: truncation of integral part (go-jsonnet + jrsonnet agree) ===
// Integer inputs
assert std.isEven(0) == true : 'isEven(0)';
assert std.isEven(2) == true : 'isEven(2)';
assert std.isEven(3) == false : 'isEven(3)';
assert std.isEven(-2) == true : 'isEven(-2)';
assert std.isEven(-3) == false : 'isEven(-3)';
// Fractional: truncation toward zero
assert std.isEven(2.7) == true : 'isEven(2.7): trunc(2.7)=2, even';
assert std.isEven(3.7) == false : 'isEven(3.7): trunc(3.7)=3, odd';
assert std.isEven(2.1) == true : 'isEven(2.1): trunc(2.1)=2, even';
assert std.isEven(3.1) == false : 'isEven(3.1): trunc(3.1)=3, odd';
// Negative fractional: truncation toward zero (not floor!)
assert std.isEven(-2.7) == true : 'isEven(-2.7): trunc(-2.7)=-2, even';
assert std.isEven(-3.7) == false : 'isEven(-3.7): trunc(-3.7)=-3, odd';
// Large integers (no precision loss)
assert std.isEven(1e20) == true : 'isEven(1e20)';

// === std.isOdd: truncation of integral part (go-jsonnet + jrsonnet agree) ===
assert std.isOdd(1) == true : 'isOdd(1)';
assert std.isOdd(2) == false : 'isOdd(2)';
assert std.isOdd(-1) == true : 'isOdd(-1)';
assert std.isOdd(-2) == false : 'isOdd(-2)';
// Fractional: truncation toward zero
assert std.isOdd(2.5) == false : 'isOdd(2.5): trunc(2.5)=2, even';
assert std.isOdd(3.5) == true : 'isOdd(3.5): trunc(3.5)=3, odd';
assert std.isOdd(2.9) == false : 'isOdd(2.9): trunc(2.9)=2, even';
assert std.isOdd(3.9) == true : 'isOdd(3.9): trunc(3.9)=3, odd';
// Negative fractional: truncation toward zero
assert std.isOdd(-2.5) == false : 'isOdd(-2.5): trunc(-2.5)=-2, even';
assert std.isOdd(-3.5) == true : 'isOdd(-3.5): trunc(-3.5)=-3, odd';
// Large integers
assert std.isOdd(1e20) == false : 'isOdd(1e20)';

// === std.isInteger: fractional part check (all 3 impls agree) ===
assert std.isInteger(0) == true : 'isInteger(0)';
assert std.isInteger(1) == true : 'isInteger(1)';
assert std.isInteger(-1) == true : 'isInteger(-1)';
assert std.isInteger(1.0) == true : 'isInteger(1.0)';
assert std.isInteger(1.1) == false : 'isInteger(1.1)';
assert std.isInteger(-1.1) == false : 'isInteger(-1.1)';
assert std.isInteger(0.5) == false : 'isInteger(0.5)';
// Large numbers: must not lose precision
assert std.isInteger(1e20) == true : 'isInteger(1e20)';
assert std.isInteger(1e15) == true : 'isInteger(1e15)';
assert std.isInteger(9007199254740992) == true : 'isInteger(2^53)';

// === std.isDecimal: complement of isInteger ===
assert std.isDecimal(0) == false : 'isDecimal(0)';
assert std.isDecimal(1) == false : 'isDecimal(1)';
assert std.isDecimal(1.1) == true : 'isDecimal(1.1)';
assert std.isDecimal(-1.1) == true : 'isDecimal(-1.1)';
assert std.isDecimal(0.5) == true : 'isDecimal(0.5)';
// Large numbers
assert std.isDecimal(1e20) == false : 'isDecimal(1e20)';
assert std.isDecimal(1e15) == false : 'isDecimal(1e15)';

// === Consistency: isInteger(x) != isDecimal(x) for all x ===
assert [std.isInteger(v) != std.isDecimal(v) for v in [0, 1, -1, 1.5, -1.5, 1e20, 1e15, 0.1, -0.1]] ==
       [true, true, true, true, true, true, true, true, true] :
       'isInteger(x) must be complement of isDecimal(x)';

true
