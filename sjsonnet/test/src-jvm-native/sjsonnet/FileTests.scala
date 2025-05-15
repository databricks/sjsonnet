package sjsonnet

import utest.*

object FileTests extends BaseFileTests {
  val skippedTests: Set[String] = Set(
    "dos_line_endings.jsonnet",
    "regex_js.jsonnet"
  ) ++ (if (isScalaNative) {
          Set(
            "stdlib.jsonnet"
          )
        } else {
          Set(
            "stdlib_native.jsonnet"
          )
        })

  val tests: Tests = Tests {
    test("files") - {
      val t = os
        .list(testSuiteRoot)
        .filter(f => !f.last.startsWith("error") && f.ext == "jsonnet")
        .filter(f => !skippedTests.contains(f.last))
      assert(t.nonEmpty)
      t.foreach { file =>
        if (file.last == "trace.jsonnet") {
          checkStderr(file)
        } else {
          check(file)
        }
      }
    }
  }
}
