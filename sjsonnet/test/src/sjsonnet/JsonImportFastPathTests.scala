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

    test("invalid unicode surrogate exceptions fall back without internal errors") {
      val inputs = Map(
        "high-before-non-low-escape.json" -> "{\"key\":\"\\uD800\\u0041\"}",
        "duplicate-high.json" -> "{\"key\":\"\\uD800\\uD801\"}",
        "lone-low.json" -> "{\"key\":\"before\\uDC00after\"}",
        "reversed.json" -> "{\"key\":\"\\uDE00\\uD83D\"}"
      )

      inputs.foreach { case (fileName, json) =>
        val result = eval(Map(fileName -> json), s"""import "$fileName"""")
        assert(result.isLeft)
        result match {
          case Left(error) =>
            assert(error.startsWith("sjsonnet.ParseError"))
            assert(!error.contains("Internal Error"))
          case Right(_)    => assert(false)
        }
      }
    }

    test("valid unicode surrogate pairs stay on the json fast path") {
      val files = Map(
        "unicode.json" -> "{\"emoji\":\"\\uD83D\\uDE00\"}"
      )

      eval(files, """import "unicode.json"""") ==>
      Right(ujson.Obj("emoji" -> "\uD83D\uDE00"))
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
