package sjsonnet

import java.io.ByteArrayOutputStream

import utest._

object RendererTests extends TestSuite {
  private def lowRecursiveMaterializeInterpreter(): Interpreter =
    new Interpreter(
      Map.empty[String, String],
      Map.empty[String, String],
      DummyPath(),
      Importer.empty,
      parseCache = new DefaultParseCache,
      settings = Settings.default.copy(materializeRecursiveDepthLimit = 1)
    )

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

    test("visitFloat64Integers") {
      ujson.transform(ujson.Num(0), new Renderer()).toString ==> "0"
      ujson.transform(ujson.Num(42), new Renderer()).toString ==> "42"
      ujson.transform(ujson.Num(-1), new Renderer()).toString ==> "-1"
      ujson.transform(ujson.Num(1e15), new Renderer()).toString ==> "1000000000000000"
      ujson.transform(ujson.Num(9999999999.0), new Renderer()).toString ==> "9999999999"
      ujson.transform(ujson.Num(Long.MaxValue.toDouble), new Renderer()).toString ==>
      Long.MaxValue.toDouble.toLong.toString
    }

    test("indentZero") {
      // indent=0 should produce newlines but no spaces
      ujson.transform(ujson.Arr(1, 2), new Renderer(indent = 0)).toString ==>
      """[
          |1,
          |2
          |]""".stripMargin
    }

    test("manifestJsonDirectFallbackKeepsCycleContext") {
      val result = lowRecursiveMaterializeInterpreter()
        .interpret("std.manifestJson({ x: self })", DummyPath("(memory)"))
      assert(
        result.left.exists(
          _.contains("Stackoverflow while materializing, possibly due to recursive value")
        )
      )
    }

    test("byteRendererDirectFallbackKeepsCycleContext") {
      val interp = lowRecursiveMaterializeInterpreter()
      val value = interp.evaluate("{ x: self }", DummyPath("(memory)")) match {
        case Right(v)  => v
        case Left(err) => throw err
      }
      val out = new ByteArrayOutputStream()
      val result = interp.materialize(value, new ByteRenderer(out, indent = -1))
      assert(
        result.left.exists(
          _.getMessage.contains(
            "Stackoverflow while materializing, possibly due to recursive value"
          )
        )
      )
    }

  }

}
