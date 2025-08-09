package sjsonnet

import utest.*

object FileTests extends BaseFileTests {
  val testDataSkippedTests: Set[String] =
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

  val goTestDataSkippedTests: Set[String] = Set.empty

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
    test("new_test_suite") - {
      val t = os
        .list(testSuiteRoot / "new_test_suite")
        .filter(f => f.ext == "jsonnet" && f.last.contains("jvm-native"))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(file, "new_test_suite")
      }
    }
  }
}
