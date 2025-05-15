package sjsonnet

import utest.*

object ErrorTests extends BaseFileTests {
  val skippedTestInScalaNative = Set(
    "error.function_infinite_default.jsonnet",
    "error.obj_recursive.jsonnet",
    "error.obj_recursive_manifest.jsonnet",
    "error.recursive_object_non_term.jsonnet",
    "error.recursive_import.jsonnet",
    "error.recursive_function_nonterm.jsonnet"
  )
  val skippedTests = Set(
    "error.decodeUTF8_float.jsonnet",
    "error.function_no_default_arg.jsonnet",
    "error.negative_shfit.jsonnet",
    "error.overflow.jsonnet",
    "error.overflow2.jsonnet",
    "error.parse.string.invalid_escape.jsonnet",
    "error.parse_json.jsonnet",
    "error.std_makeArray_negative.jsonnet"
  ) ++ (
    if (isScalaNative) {
      skippedTestInScalaNative
    } else {
      Set()
    }
  )

  val tests: Tests = Tests {
    test("error") - {
      val t = os
        .list(testSuiteRoot)
        .filter(f => f.last.startsWith("error") && f.ext == "jsonnet")
        .filter(f => !skippedTests.contains(f.last))
      assert(t.nonEmpty)
      t.foreach { file =>
        checkError(file)
      }
    }
  }
}
