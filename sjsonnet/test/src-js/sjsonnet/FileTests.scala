package sjsonnet

import utest._

object FileTests extends BaseFileTests {
  val skippedTests = Set(
    "stdlib.jsonnet",
    "regex.jsonnet",
    "dos_line_endings.jsonnet",
    "recursive_function.jsonnet",

    // Stack size issues with the JS runner
    "error.array_recursive_manifest.jsonnet",
    "error.function_infinite_default.jsonnet",
    "error.obj_recursive.jsonnet",
    "error.obj_recursive_manifest.jsonnet",
    "error.recursive_object_non_term.jsonnet",
    "error.recursive_import.jsonnet",
    "error.recursive_function_nonterm.jsonnet",
    "error.array_large_index.jsonnet"
  )

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
    "tailstrict3.jsonnet",
    "stdlib_smoke_test.jsonnet",
    "builtinSha1.jsonnet",
    "builtinSha256.jsonnet",
    "builtinSha3.jsonnet",
    "builtinSha512.jsonnet",
    "std.md5.jsonnet",
    "std.md5_2.jsonnet",
    "std.md5_3.jsonnet",
    "std.md5_4.jsonnet",
    "std.md5_5.jsonnet"
  )

  val tests: Tests = Tests {
    test("test_suite") - {
      val t = TestResources_test_suite.files.keys.toSeq.sorted
        .filter(f => f.matches("[^/]+\\.jsonnet"))
        .filter(f => !skippedTests.contains(f))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(TestResources_test_suite.files, file, "test_suite")
      }
    }

    test("go_test_suite") - {
      val t = TestResources_go_test_suite.files.keys.toSeq.sorted
        .filter(f => f.matches("[^/]+\\.jsonnet"))
        .filter(f => !goTestDataSkippedTests.contains(f))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(TestResources_go_test_suite.files, file, "go_test_suite")
      }
    }
  }
}
