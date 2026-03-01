package sjsonnet

import utest.*

object ConfigTests extends TestSuite {
  private val sep = java.io.File.pathSeparator

  val tests: Tests = Tests {
    test("jsonnetPathEntries") {
      test("null") {
        assert(Config.jsonnetPathEntries(null) == Nil)
      }
      test("empty") {
        assert(Config.jsonnetPathEntries("") == Nil)
      }
      test("single") {
        // Single path should be returned as-is
        assert(Config.jsonnetPathEntries("/foo") == Seq("/foo"))
      }
      test("multiple") {
        // JSONNET_PATH=a<sep>b should produce Seq("a", "b") (original order, left-most wins)
        val result = Config.jsonnetPathEntries(s"/a${sep}/b${sep}/c")
        assert(result == Seq("/a", "/b", "/c"))
      }
      test("emptyEntries") {
        // Empty entries between separators should be filtered out
        val result = Config.jsonnetPathEntries(s"/a$sep$sep/b")
        assert(result == Seq("/a", "/b"))
      }
      test("trailingSeparator") {
        val result = Config.jsonnetPathEntries(s"/a$sep/b$sep")
        assert(result == Seq("/a", "/b"))
      }
    }

    test("getOrderedJpaths") {
      test("jpathsOnlyDefaultOrder") {
        // Without JSONNET_PATH env set, getOrderedJpaths should return jpaths in original order
        val config = Config(jpaths = List("/x", "/y", "/z"), file = "test.jsonnet")
        val result = config.getOrderedJpaths(jsonnetPathEnv = Some(""))
        assert(result == Seq("/x", "/y", "/z"))
      }
      test("jpathsOnlyReversed") {
        import mainargs.Flag
        val config = Config(
          jpaths = List("/x", "/y", "/z"),
          reverseJpathsPriority = Flag(true),
          file = "test.jsonnet"
        )
        val result = config.getOrderedJpaths(jsonnetPathEnv = Some(""))
        assert(result == Seq("/z", "/y", "/x"))
      }
      test("envPathsAppendedAfterJpaths") {
        val config = Config(jpaths = List("/c", "/d"), file = "test.jsonnet")
        val result = config.getOrderedJpaths(jsonnetPathEnv = Some(s"/a$sep/b"))
        // -J paths first, then JSONNET_PATH entries in original order
        assert(result == Seq("/c", "/d", "/a", "/b"))
      }
      test("envPathsWithReverse") {
        import mainargs.Flag
        val config = Config(
          jpaths = List("/c", "/d"),
          reverseJpathsPriority = Flag(true),
          file = "test.jsonnet"
        )
        val result = config.getOrderedJpaths(jsonnetPathEnv = Some(s"/a$sep/b"))
        // reversed -J paths first, then JSONNET_PATH entries in original order
        assert(result == Seq("/d", "/c", "/a", "/b"))
      }
      test("envPathsOnlyNoJpaths") {
        val config = Config(file = "test.jsonnet")
        val result = config.getOrderedJpaths(jsonnetPathEnv = Some(s"/a$sep/b"))
        assert(result == Seq("/a", "/b"))
      }
      test("noArgsDefaultFallback") {
        // Use the injected overload with an empty env string so the test is
        // deterministic even when the real JSONNET_PATH env var is set.
        val config = Config(jpaths = List("/x"), file = "test.jsonnet")
        val result = config.getOrderedJpaths(jsonnetPathEnv = Some(""))
        assert(result == Seq("/x"))
      }
    }
  }
}
