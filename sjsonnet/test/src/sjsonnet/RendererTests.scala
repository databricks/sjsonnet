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

  }

}
