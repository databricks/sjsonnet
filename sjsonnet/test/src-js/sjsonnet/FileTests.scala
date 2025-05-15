package sjsonnet

import utest._

object FileTests extends BaseFileTests {
  val skippedTests = Set(
    "test_suite/dos_line_endings.jsonnet",
    "test_suite/stdlib.jsonnet",
    "test_suite/recursive_function.jsonnet",
    "test_suite/regex.jsonnet",
    "test_suite/trace.jsonnet"
  )

  val tests: Tests = Tests {
    test("files") - {
      val t = TestResources.files.keys.toSeq.sorted
        .filter(f => f.matches("test_suite/[A-Za-z0-9_-]+\\.jsonnet"))
        .filter(f => !skippedTests.contains(f))
      assert(t.nonEmpty)
      t.foreach { file =>
        check(file)
      }
    }
  }
}
