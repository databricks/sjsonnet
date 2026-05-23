package sjsonnet

import java.io.ByteArrayOutputStream
import utest._

object RendererTests extends TestSuite {
  def tests: Tests = Tests {
    test("hello") {
      ujson.transform(ujson.Arr(ujson.Num(1), ujson.Num(2)), new Renderer()).toString ==>
      "[1, 2]"
    }
    test("empty") {
      ujson.transform(ujson.Arr(), new Renderer(indent = 3)).toString ==> "[ ]"
      ujson.transform(ujson.Obj(), new Renderer(indent = 3)).toString ==> "{ }"
    }
    test("nonEmpty") {
      ujson.transform(ujson.Arr(1), new Renderer(indent = 3)).toString ==>
      """[
        |   1
        |]""".stripMargin
      ujson.transform(ujson.Arr(1, 2), new Renderer(indent = 3)).toString ==>
      """[
        |   1,
        |   2
        |]""".stripMargin
      ujson.transform(ujson.Obj("a" -> 1), new Renderer(indent = 3)).toString ==>
      """{
        |   "a": 1
        |}""".stripMargin
      ujson.transform(ujson.Obj("a" -> 1, "b" -> 2), new Renderer(indent = 3)).toString ==>
      """{
        |   "a": 1,
        |   "b": 2
        |}""".stripMargin
    }
    test("nested") {
      ujson
        .transform(ujson.Arr(ujson.Obj("a" -> ujson.Arr())), new Renderer(indent = 3))
        .toString ==>
      """[
        |   {
        |      "a": [ ]
        |   }
        |]""".stripMargin
    }

    test("appendLong") {
      def render(v: Long): String = {
        val cb = new upickle.core.CharBuilder
        RenderUtils.appendLong(cb, v)
        cb.makeString()
      }
      test("zero") { render(0L) ==> "0" }
      test("positive") { render(42L) ==> "42" }
      test("negative") { render(-1L) ==> "-1" }
      test("large") { render(9999999999L) ==> "9999999999" }
      test("maxValue") { render(Long.MaxValue) ==> Long.MaxValue.toString }
      test("minValue") { render(Long.MinValue) ==> Long.MinValue.toString }
    }

    test("visitFloat64Integers") {
      // Verify that integer-valued doubles render correctly via the Renderer
      ujson.transform(ujson.Num(0), new Renderer()).toString ==> "0"
      ujson.transform(ujson.Num(42), new Renderer()).toString ==> "42"
      ujson.transform(ujson.Num(-1), new Renderer()).toString ==> "-1"
      ujson.transform(ujson.Num(1e15), new Renderer()).toString ==> "1000000000000000"
    }

    test("byteRendererFallbackPreservesCycleContext") {
      val interpreter = new Interpreter(
        Map(),
        Map(),
        DummyPath(),
        Importer.empty,
        parseCache = new DefaultParseCache,
        settings = Settings.default.copy(materializeRecursiveDepthLimit = 2, maxMaterializeDepth = 2)
      )
      val value = interpreter.evaluate(
        """local o = {
          |  a: std.repeat("x", 10000),
          |  z: { b: o },
          |};
          |{ root: o }""".stripMargin,
        DummyPath("(memory)")
      ) match {
        case Right(v)  => v
        case Left(err) => throw new Exception(Error.formatError(err))
      }
      val out = new ByteArrayOutputStream
      val e = interpreter.materialize(value, new ByteRenderer(out)) match {
        case Left(err) => Error.formatError(err)
        case Right(_)  => throw new Exception("Expected recursive value materialization error")
      }
      assert(e.contains("Stackoverflow while materializing, possibly due to recursive value"))
      // Losing the outer materialization context re-renders `o.a` before detecting the cycle.
      assert(out.size() < 15000)
    }

    test("indentZero") {
      // indent=0 should produce newlines but no spaces
      ujson.transform(ujson.Arr(1, 2), new Renderer(indent = 0)).toString ==>
      """[
          |1,
          |2
          |]""".stripMargin
    }

    test("escapeBulkFastPath") {
      // Verifies the BaseRenderer.escape bulk-write chunked path: when a String has no chars
      // requiring escaping, output is byte-identical to the slow per-char path. Mirrors
      // the conditions used by TomlRenderer/YamlRenderer/escapeStringJson.
      def escape(s: CharSequence, unicode: Boolean): String = {
        val w = new java.io.StringWriter()
        BaseRenderer.escape(w, s, unicode)
        w.toString
      }

      // ASCII-safe inputs hit the bulk path; outputs must be `"<input>"` verbatim.
      escape("", unicode = true) ==> "\"\""
      escape("hello", unicode = true) ==> "\"hello\""
      escape("Plain ASCII 0-9 ~!@#$%^&*()", unicode = true) ==> "\"Plain ASCII 0-9 ~!@#$%^&*()\""
      // A long string ensures we actually exercise the bulk-write path.
      val long = "x" * 4096
      escape(long, unicode = true) ==> "\"" + long + "\""

      // All named escape mappings.
      escape("a\"b", unicode = true) ==> "\"a\\\"b\""
      escape("a\\b", unicode = true) ==> "\"a\\\\b\""
      escape("a\bb", unicode = true) ==> "\"a\\bb\""
      escape("a\fb", unicode = true) ==> "\"a\\fb\""
      escape("a\nb", unicode = true) ==> "\"a\\nb\""
      escape("a\rb", unicode = true) ==> "\"a\\rb\""
      escape("a\tb", unicode = true) ==> "\"a\\tb\""

      // Control chars that fall through to unicode escapes.
      escape("\u0000", unicode = true) ==> "\"\\u0000\""
      escape("\u0001", unicode = true) ==> "\"\\u0001\""
      escape("\u001f", unicode = true) ==> "\"\\u001f\""

      // 0x20 (space) is the lowest safe char; 0x7E (~) is the highest ASCII safe char.
      escape(" ", unicode = true) ==> "\" \""
      escape("~", unicode = true) ==> "\"~\""

      // 0x7F (DEL): escaped under unicode=true, but passes through under unicode=false.
      escape("\u007f", unicode = true) ==> "\"\\u007f\""
      escape("\u007f", unicode = false) ==> "\"\u007f\""

      // Higher BMP: \u00ff escaped under unicode=true, passes through under unicode=false.
      escape("\u00ff", unicode = true) ==> "\"\\u00ff\""
      escape("\u00ff", unicode = false) ==> "\"\u00ff\""

      // U+2028 / U+2029 (JS-specific line separators) — pinned to current behaviour: escaped
      // only when unicode=true. Old per-char path behaved the same way.
      escape("\u2028", unicode = false) ==> "\"\u2028\""
      escape("\u2028", unicode = true) ==> "\"\\u2028\""
      escape("\u2029", unicode = true) ==> "\"\\u2029\""

      // Surrogate pair (emoji 😀 = U+1F600) → \ud83d\ude00 when unicode=true; pass-through
      // bytes preserved when unicode=false.
      escape("\uD83D\uDE00", unicode = true) ==> "\"\\ud83d\\ude00\""
      escape("\uD83D\uDE00", unicode = false) ==> "\"\uD83D\uDE00\""

      // Consecutive unsafe chars exercise the `if (i > start)` zero-length guard.
      escape("\"\\", unicode = true) ==> "\"\\\"\\\\\""
      escape("\n\t", unicode = true) ==> "\"\\n\\t\""

      // Unsafe char at start and end exercise leading/trailing chunk boundaries.
      escape("\nabc", unicode = true) ==> "\"\\nabc\""
      escape("abc\n", unicode = true) ==> "\"abc\\n\""

      // Mixed alternating safe/unsafe runs.
      escape("abc\nDEF\u00ffghi", unicode = true) ==> "\"abc\\nDEF\\u00ffghi\""

      // Non-String CharSequence routes to the `escapeChars` per-char path; output must match.
      val sb = new java.lang.StringBuilder("a\nb")
      escape(sb, unicode = true) ==> "\"a\\nb\""
      escape(new java.lang.StringBuilder("plain"), unicode = true) ==> "\"plain\""
    }

  }

}
