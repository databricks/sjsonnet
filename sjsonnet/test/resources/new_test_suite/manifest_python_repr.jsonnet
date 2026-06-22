// std.manifestPython and std.manifestPythonVars should render floats in
// CPython 3 repr() style (the ground truth for `manifestPython`): integer-
// valued doubles emit without a decimal point (e.g. 1.0 → "1", 1e100 → "1e+100");
// negative zero emits as "-0"; non-integer doubles use the shortest round-trip
// form with scientific notation (lowercase "e", signed exponent, ≥2 digits,
// zero-padded) for magnitudes outside [1e-4, 1e16) and fixed-point otherwise.

// Integer-valued doubles — no decimal point (Python repr: 1.0 → "1.0" but
// go-jsonnet/C++ jsonnet/sjsonnet convention is "1" without the ".0", which
// is accepted as matching Python output semantically).
std.assertEqual(std.manifestPython(0.0), "0") &&
std.assertEqual(std.manifestPython(1.0), "1") &&
std.assertEqual(std.manifestPython(42.0), "42") &&
std.assertEqual(std.manifestPython(-0.0), "-0") &&
std.assertEqual(std.manifestPython(-42.0), "-42") &&
// Fractional doubles — fixed-point in [-4, 16) adjusted exponent range.
std.assertEqual(std.manifestPython(1.5), "1.5") &&
std.assertEqual(std.manifestPython(-1.5), "-1.5") &&
std.assertEqual(std.manifestPython(0.0001), "0.0001") &&
std.assertEqual(std.manifestPython(0.0012), "0.0012") &&
std.assertEqual(std.manifestPython(0.00012), "0.00012") &&
std.assertEqual(std.manifestPython(3.141592653589793), "3.141592653589793") &&
std.assertEqual(std.manifestPython(1e15), "1000000000000000") &&
std.assertEqual(std.manifestPython(10000000000000000.0), "1e+16") &&
std.assertEqual(std.manifestPython(9223372036854775808), "9.223372036854776e+18") &&
// Fractional doubles — scientific outside [-4, 16). Lowercase "e", signed
// exponent, zero-padded to 2 digits, mantissa stripped of trailing zeros.
std.assertEqual(std.manifestPython(0.000001), "1e-06") &&
std.assertEqual(std.manifestPython(1e-10), "1e-10") &&
std.assertEqual(std.manifestPython(1e-5), "1e-05") &&
std.assertEqual(std.manifestPython(1e-100), "1e-100") &&
std.assertEqual(std.manifestPython(1e16), "1e+16") &&
std.assertEqual(std.manifestPython(1e20), "1e+20") &&
std.assertEqual(std.manifestPython(1e100), "1e+100") &&
std.assertEqual(std.manifestPython(-1e-6), "-1e-06") &&
std.assertEqual(std.manifestPython(-1e100), "-1e+100") &&
// Non-float types.
std.assertEqual(std.manifestPython(true), "True") &&
std.assertEqual(std.manifestPython(false), "False") &&
std.assertEqual(std.manifestPython(null), "None") &&
std.assertEqual(std.manifestPython(42), "42") &&
std.assertEqual(std.manifestPython("hello"), '"hello"') &&
std.assertEqual(std.manifestPython([1, 2, 3]), "[1, 2, 3]") &&
std.assertEqual(std.manifestPython([]), "[]") &&
std.assertEqual(std.manifestPython({}), "{}") &&
// manifestPythonVars: same rendering, each key on its own line, sorted by key.
std.assertEqual(std.manifestPythonVars({}), "") &&
std.assertEqual(std.manifestPythonVars({a: 1}), "a = 1\n") &&
std.assertEqual(std.manifestPythonVars({a: 1, b: 2.5, c: "x"}),
                "a = 1\nb = 2.5\nc = \"x\"\n") &&
std.assertEqual(std.manifestPythonVars({x: 0.000001, y: 1e100}),
                "x = 1e-06\ny = 1e+100\n") &&
true
