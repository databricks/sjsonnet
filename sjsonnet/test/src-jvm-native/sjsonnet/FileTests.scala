package sjsonnet

import utest.*

object FileTests extends BaseFileTests {
  val testDataSkippedTests: Set[String] = {
    val nativeOnly =
      if (isScalaNative)
        Set(
          // These tests are skipped in Scala Native because we can't catch the stack overflow and recover.
          "error.obj_recursive_manifest.jsonnet",
          "error.recursive_object_non_term.jsonnet",
          "error.recursive_import.jsonnet",
          "error.recursive_function_nonterm.jsonnet",
          "error.function_infinite_default.jsonnet",
          "error.obj_recursive.jsonnet"
        )
      else Set.empty[String]
    nativeOnly ++ Set(
      // ParseError is thrown directly (not caught by Interpreter.interpret) for deeply nested arrays,
      // causing the test framework to crash rather than producing a Left error result.
      "error.parse.deep_array_nesting.jsonnet"
    )
  }

  val goTestDataSkippedTests: Set[String] = Set(
    // We support base64 of unicode strings
    "builtinBase64_string_high_codepoint.jsonnet"
  )

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
      printSummaryAndAssert()
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
      printSummaryAndAssert()
    }
    test("new_test_suite") - {
      val t = os
        .list(testSuiteRoot / "new_test_suite")
        .filter(f => f.ext == "jsonnet" && !f.last.contains("-js"))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(file, "new_test_suite")
      }
      printSummaryAndAssert()
    }
  }
}
