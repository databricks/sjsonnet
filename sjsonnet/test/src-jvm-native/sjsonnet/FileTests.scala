package sjsonnet

import utest.*

object FileTests extends BaseFileTests {
  val testDataSkippedTests: Set[String] = Set(
    "dos_line_endings.jsonnet",
    "regex_js.jsonnet",
    "stdlib_js.jsonnet"
  ) ++ (if (isScalaNative) {
          Set(
            "error.obj_recursive_manifest.jsonnet",
            "error.recursive_object_non_term.jsonnet",
            "error.recursive_import.jsonnet",
            "error.recursive_function_nonterm.jsonnet",
            "error.function_infinite_default.jsonnet",
            "error.obj_recursive.jsonnet"
          )
        } else {
          Set.empty[String]
        })
  val goTestDataSkippedTests: Set[String] = Set(
    "bitwise_or9.jsonnet",
    "builtinChar6.jsonnet",
    "builtin_escapeStringJson.jsonnet",
    "builtin_manifestTomlEx.jsonnet",
    "escaped_fields.jsonnet",
    "multi.jsonnet",
    "multi_string_output.jsonnet",
    "number_leading_zero.jsonnet",
    "object_comp_assert.jsonnet",
    "object_comp_bad_field.jsonnet",
    "object_comp_bad_field2.jsonnet",
    "object_comp_illegal.jsonnet",
    "pow6.jsonnet",
    "object_invariant_plus.jsonnet",
    "std.makeArray_recursive_evalutation_order_matters.jsonnet",
    "tailstrict3.jsonnet"
  ) ++ (if (isScalaNative)
          Set(
            "stdlib_smoke_test.jsonnet",
            "builtinSha3.jsonnet"
          )
        else Set.empty[String])

  val tests: Tests = Tests {
    test("test_suite") - {
      val t = os
        .list(testSuiteRoot / "test_suite")
        .filter(f => f.ext == "jsonnet")
        .filter(f => !testDataSkippedTests.contains(f.last))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(file, "test_suite")
      }
    }
    test("go_test_suite") - {
      val t = os
        .list(testSuiteRoot / "go_test_suite")
        .filter(f => f.ext == "jsonnet")
        .filter(f => !goTestDataSkippedTests.contains(f.last))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(file, "go_test_suite")
      }
    }
  }
}
