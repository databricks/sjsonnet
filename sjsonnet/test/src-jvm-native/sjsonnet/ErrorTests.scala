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

  val skippedTests = if (isScalaNative) {
    skippedTestInScalaNative
  } else {
    Set.empty[String]
  }

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
