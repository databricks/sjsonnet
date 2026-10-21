package sjsonnet

import utest._

object JsonImportFastPathTests extends TestSuite {
  private def interpreter(files: Map[String, String], settings: Settings): Interpreter =
    new Interpreter(
      Map.empty,
      Map.empty,
      DummyPath("root"),
      new Importer {
        def resolve(docBase: Path, importName: String): Option[Path] =
          if (files.contains(importName)) Some(DummyPath(importName)) else None
        def read(path: Path, binaryData: Boolean): Option[ResolvedFile] =
          path match {
            case DummyPath(name) => files.get(name).map(StaticResolvedFile.apply)
            case _               => None
          }
      },
      parseCache = new DefaultParseCache,
      settings = settings
    )

  private def eval(
      files: Map[String, String],
      code: String,
      settings: Settings = Settings.default): Either[String, ujson.Value] =
    interpreter(files, settings).interpret(code, DummyPath("root", "main.jsonnet"))

  private def assertSorted(keys: Array[String]): Unit = {
    var i = 1
    while (i < keys.length) {
      assert(Util.compareStringsByCodepoint(keys(i - 1), keys(i)) < 0)
      i += 1
    }
  }

  private def unicodeEscape(hex: String): String = "\\" + "u" + hex

  private def assertInvalidUnicodeImport(json: String): Unit = {
    val result = eval(Map("surrogate.json" -> json), """import "surrogate.json"""")
    assert(result.isLeft)
    result match {
      case Left(error) =>
        assert(error.contains("Invalid JSON: unpaired surrogate in string"))
        assert(!error.contains("Internal Error"))
      case Right(_) => assert(false)
    }
  }

  def tests: Tests = Tests {
    test("strict json imports produce normal Jsonnet values") {
      val files = Map(
        "data.json" ->
        """{"a":[1,true,null],"b":{"c":"d"},"n":9.007199254740992E15}"""
      )

      eval(files, """import "data.json"""") ==>
      Right(
        ujson.Obj(
          "a" -> ujson.Arr(1, true, ujson.Null),
          "b" -> ujson.Obj("c" -> "d"),
          "n" -> 9.007199254740992e15
        )
      )
    }

    test("profiler handles strict json imports without source offsets") {
      val interp = interpreter(Map("data.json" -> """{"a":1}"""), Settings.default)
      val profiler = new Profiler(ProfileOutputFormat.Text, DummyPath("root"))
      interp.evaluator.profiler = profiler

      interp.interpret("""import "data.json"""", DummyPath("root", "main.jsonnet")) ==>
      Right(ujson.Obj("a" -> 1))

      profiler.initLineColLookup(pos => interp.evaluator.prettyIndex(pos))
      val output = ProfilePrinter.formatResult(profiler.collectResult()).mkString("\n")
      assert(output.contains(":?:? Obj"))
    }

    test("strict json import sorted key caches are lazy and reusable") {
      val fields = (0 until 20).map(i => s""""k${19 - i}":$i""").mkString("{", ",", "}")
      val obj = interpreter(Map("data.json" -> fields), Settings.default)
        .evaluate("""import "data.json"""", DummyPath("root", "main.jsonnet")) match {
        case Right(v)  => v.asObj
        case Left(err) => throw new Exception(Error.formatError(err))
      }

      assert(obj._sortedVisibleKeyNames == null)
      assert(obj._sortedAllKeyNames == null)

      val visible = obj.sortedVisibleKeyNames
      val all = obj.sortedAllKeyNames
      assert(visible != null)
      assert(all != null)
      assert(visible.length == 20)
      assert(all.length == 20)
      assert(visible.sameElements(all))
      assertSorted(visible)
      assertSorted(all)
      assert(obj.sortedVisibleKeyNames eq visible)
      assert(obj.sortedAllKeyNames eq all)
    }

    test("invalid json can still fall back to Jsonnet syntax") {
      val files = Map(
        "loose.json" ->
        """// Valid Jsonnet, invalid strict JSON.
            |{
            |  a: 1,
            |}
            |""".stripMargin
      )

      eval(files, """import "loose.json"""") ==> Right(ujson.Obj("a" -> 1))
    }

    test("surrogate escapes in Jsonnet fallback comments do not fail json import") {
      val files = Map(
        "loose-surrogate-comment.json" ->
        s"""// "${unicodeEscape("D800")}"
           |{
           |  a: 1,
           |}
           |""".stripMargin
      )

      eval(files, """import "loose-surrogate-comment.json"""") ==> Right(ujson.Obj("a" -> 1))
    }

    test("duplicate json object keys keep Jsonnet duplicate-field error semantics") {
      val files = Map("dupe.json" -> """{"a":1,"a":2}""")

      val result = eval(files, """import "dupe.json"""")
      assert(result.isLeft)
      result match {
        case Left(error) => assert(error.contains("duplicate") || error.contains("Duplicate"))
        case Right(_)    => assert(false)
      }
    }

    test("large integer json numbers keep Jsonnet double semantics") {
      val files = Map("large-int.json" -> """{"n":18446744073709551615}""")

      eval(files, """import "large-int.json"""") ==>
      Right(ujson.Obj("n" -> 1.8446744073709552e19))
    }

    test("non-finite json numbers keep Jsonnet parser errors") {
      val files = Map("overflow.json" -> """{"n":1e10000}""")

      val result = eval(files, """import "overflow.json"""")
      assert(result.isLeft)
      result match {
        case Left(error) => assert(error.contains("finite number required"))
        case Right(_)    => assert(false)
      }
    }

    test("incomplete json falls back to normal parse errors") {
      val files = Map("truncated.json" -> """{"a":[1""")

      val result = eval(files, """import "truncated.json"""")
      assert(result.isLeft)
      result match {
        case Left(error) => assert(!error.contains("Internal Error"))
        case Right(_)    => assert(false)
      }
    }

    test("unpaired surrogate json imports fail before producing Jsonnet strings") {
      assertInvalidUnicodeImport(s"""{"s":"${unicodeEscape("D800")}"}""")
      assertInvalidUnicodeImport(s"""{"s":"${unicodeEscape("DC00")}"}""")
      assertInvalidUnicodeImport(s""""${unicodeEscape("D800")}"""")
      assertInvalidUnicodeImport(s""""${unicodeEscape("DC00")}"""")
    }

    test("unpaired surrogate error position stays char-accurate after multi-byte utf8") {
      // "é" is 2 bytes in UTF-8: the bad escape's backslash sits at char 6 (1-based column 7).
      // A byte-based offset would drift to column 8.
      val files = Map("surrogate.json" -> s"""{"é":"${unicodeEscape("D800")}"}""")
      val result = eval(files, """import "surrogate.json"""")
      assert(result.isLeft)
      result match {
        case Left(error) =>
          assert(error.contains("Invalid JSON: unpaired surrogate in string"))
          assert(error.contains("at line 1 column 7"))
        case Right(_) => assert(false)
      }
    }

    test("valid surrogate pair json imports keep codepoint strings") {
      val pairEscape = unicodeEscape("D83D") + unicodeEscape("DE00")
      val files = Map("surrogate-pair.json" -> s"""{"s":"$pairEscape","$pairEscape":"key"}""")

      eval(files, """local obj = import "surrogate-pair.json"; [std.codepoint(obj.s), obj[std.char(128512)]]""") ==>
      Right(ujson.Arr(128512, "key"))
    }

    test("escaped surrogate text json imports stay ordinary strings") {
      val escapedText = "\\" + unicodeEscape("D800")
      val files = Map(
        "escaped-surrogate-text.json" -> s"""{"s":"$escapedText"}""",
        "escaped-surrogate-text-with-suffix.json" -> s"""{"s":"${escapedText}abc"}"""
      )

      eval(files, """(import "escaped-surrogate-text.json").s""") ==>
      Right(ujson.Str(unicodeEscape("D800")))
      eval(files, """(import "escaped-surrogate-text-with-suffix.json").s""") ==>
      Right(ujson.Str(unicodeEscape("D800") + "abc"))
    }

    test("deep json imports keep parser recursion guard") {
      val files = Map("deep.json" -> """[[[]]]""")

      val result = eval(
        files,
        """import "deep.json"""",
        Settings.default.copy(maxParserRecursionDepth = 1)
      )
      assert(result.isLeft)
      result match {
        case Left(error) => assert(error.contains("maximum recursion depth"))
        case Right(_)    => assert(false)
      }
    }
  }
}
