package sjsonnet

import utest._

object FileTests extends BaseFileTests {
  val skippedTests = Set(
    // Stack size issues with the JS runner
    "recursive_function.jsonnet",
    "error.array_recursive_manifest.jsonnet",
    "error.obj_recursive_manifest.jsonnet",
    "error.recursive_object_non_term.jsonnet",
    "error.recursive_import.jsonnet",
    "error.recursive_function_nonterm.jsonnet",
    "error.function_infinite_default.jsonnet",
    "error.obj_recursive.jsonnet"
  )

  val goTestDataSkippedTests: Set[String] = Set(
    // We support base64 of unicode strings
    "builtinBase64_string_high_codepoint.jsonnet",
    "builtinSha1.jsonnet",
    "builtinSha256.jsonnet",
    "builtinSha3.jsonnet",
    "builtinSha512.jsonnet",
    "std.md5.jsonnet",
    "std.md5_2.jsonnet",
    "std.md5_3.jsonnet",
    "std.md5_4.jsonnet",
    "std.md5_5.jsonnet",
    "std.md5_6.jsonnet"
  )

  val tests: Tests = Tests {
    test("test_suite") - {
      val t = TestResources_test_suite.files.keys.toSeq
        .filter(f => f.matches("[^/]+\\.jsonnet"))
        .filter(f => !skippedTests.contains(f))
        .sorted
      assert(t.nonEmpty)
      t.foreach { file =>
        check(TestResources_test_suite.files, file, "test_suite")
      }
    }

    test("go_test_suite") - {
      val t = TestResources_go_test_suite.files.keys.toSeq
        .filter(f => f.matches("[^/]+\\.jsonnet"))
        .filter(f => !goTestDataSkippedTests.contains(f))
        .sorted
      assert(t.nonEmpty)
      t.foreach { file =>
        check(TestResources_go_test_suite.files, file, "go_test_suite")
      }
    }

    test("new_test_suite") - {
      val t = TestResources_new_test_suite.files.keys.toSeq
        .filter(f =>
          f.matches("[^/]+-js\\.jsonnet") || (f.matches("[^/]+\\.jsonnet") && !f.contains("-jvm"))
        )
        .sorted
      assert(t.nonEmpty)
      t.foreach { file =>
        check(TestResources_new_test_suite.files, file, "new_test_suite")
      }
    }
  }
}
