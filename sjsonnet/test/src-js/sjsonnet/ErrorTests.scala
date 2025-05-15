package sjsonnet

import utest._

object ErrorTests extends BaseFileTests {
  val skippedTests = Set(
    "test_suite/error.decodeUTF8_float.jsonnet",
    "test_suite/error.function_no_default_arg.jsonnet",
    "test_suite/error.negative_shfit.jsonnet",
    "test_suite/error.overflow.jsonnet",
    "test_suite/error.overflow2.jsonnet",
    "test_suite/error.parse.string.invalid_escape.jsonnet",
    "test_suite/error.parse_json.jsonnet",
    "test_suite/error.std_makeArray_negative.jsonnet",
    "test_suite/error.array_recursive_manifest.jsonnet",
    "test_suite/error.function_infinite_default.jsonnet",
    "test_suite/error.obj_recursive.jsonnet",
    "test_suite/error.obj_recursive_manifest.jsonnet",
    "test_suite/error.recursive_object_non_term.jsonnet",
    "test_suite/error.recursive_import.jsonnet",
    "test_suite/error.recursive_function_nonterm.jsonnet",
    "test_suite/error.array_large_index.jsonnet"
  )

  val tests: Tests = Tests {
    test("error") - {
      val t = TestResources.files.keys.toSeq.sorted
        .filter(f => f.matches("test_suite/error\\.[A-Za-z0-9_-]+\\.jsonnet"))
        .filter(f => !skippedTests.contains(f))
      assert(t.nonEmpty)
      t.foreach { file =>
        checkError(file)
      }
    }
  }
}
