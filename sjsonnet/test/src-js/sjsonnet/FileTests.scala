package sjsonnet

import utest._

object FileTests extends BaseFileTests {
  val skippedTests = Set(
    "test_suite/stdlib.jsonnet",
    "test_suite/regex.jsonnet",
    "test_suite/dos_line_endings.jsonnet",
    "test_suite/recursive_function.jsonnet",

    // Stack size issues with the JS runner
    "test_suite/error.array_recursive_manifest.jsonnet",
    "test_suite/error.function_infinite_default.jsonnet",
    "test_suite/error.obj_recursive.jsonnet",
    "test_suite/error.obj_recursive_manifest.jsonnet",
    "test_suite/error.recursive_object_non_term.jsonnet",
    "test_suite/error.recursive_import.jsonnet",
    "test_suite/error.recursive_function_nonterm.jsonnet",
    "test_suite/error.array_large_index.jsonnet"
  )

  val goTestDataSkippedTests: Set[String] = Set(
    "bitwise_or9.jsonnet",
    "builtinChar6.jsonnet",
    "builtin_escapeStringJson.jsonnet",
    "builtin_manifestTomlEx.jsonnet",
    "escaped_fields.jsonnet",
    "extvar_code.jsonnet",
    "extvar_hermetic.jsonnet",
    "extvar_mutually_recursive.jsonnet",
    "extvar_not_a_string.jsonnet",
    "extvar_self_recursive.jsonnet",
    "extvar_static_error.jsonnet",
    "extvar_string.jsonnet",
    "multi.jsonnet",
    "multi_string_output.jsonnet",
    "native1.jsonnet",
    "native2.jsonnet",
    "native3.jsonnet",
    "native6.jsonnet",
    "number_leading_zero.jsonnet",
    "object_comp_assert.jsonnet",
    "object_comp_bad_field.jsonnet",
    "object_comp_bad_field2.jsonnet",
    "object_comp_illegal.jsonnet",
    "pow6.jsonnet",
    "native4.jsonnet",
    "native5.jsonnet",
    "native7.jsonnet",
    "native_error.jsonnet",
    "native_panic.jsonnet",
    "object_invariant_plus.jsonnet",
    "std.makeArray_recursive_evalutation_order_matters.jsonnet",
    "tailstrict3.jsonnet",
    "std.exponent6.jsonnet",
    "std.exponent7.jsonnet",
    "std.mantissa3.jsonnet",
    "std.mantissa6.jsonnet",
    "std.mantissa7.jsonnet",
    "stdlib_smoke_test.jsonnet",
    "builtinSha1.jsonnet",
    "builtinSha256.jsonnet",
    "builtinSha3.jsonnet",
    "builtinSha512.jsonnet",
    "parseYaml.jsonnet",
    "std.md5.jsonnet",
    "std.md5_2.jsonnet",
    "std.md5_3.jsonnet",
    "std.md5_4.jsonnet",
    "std.md5_5.jsonnet"
  )

  val tests: Tests = Tests {
    test("test_suite") - {
      val t = TestResources.files.keys.toSeq.sorted
        .filter(f => f.matches("test_suite/[^/]+\\.jsonnet"))
        .filter(f => !skippedTests.contains(f))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(file, "test_suite")
      }
    }

    test("go_test_suite") - {
      val t = TestResources.files.keys.toSeq.sorted
        .filter(f => f.matches("go_test_suite/[^/]+\\.jsonnet"))
        .filter(f => !goTestDataSkippedTests.contains(f.substring("go_test_suite/".length)))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(file, "go_test_suite")
      }
    }
  }
}
