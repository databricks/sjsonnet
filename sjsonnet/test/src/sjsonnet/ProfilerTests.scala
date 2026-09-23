package sjsonnet

import utest._

object ProfilerTests extends TestSuite {
  private val code =
    """local f(x) = std.length(x) == std.length(std.objectFields({ a: 1, b: 2 }));
      |f([1, 2])""".stripMargin

  private def interpreter(): Interpreter =
    new Interpreter(
      Map.empty,
      Map.empty,
      DummyPath("root"),
      Importer.empty,
      parseCache = new DefaultParseCache
    )

  def tests: Tests = Tests {
    test("synthetic static values have unknown source files") {
      Seq(ProfileOutputFormat.Text, ProfileOutputFormat.FlameGraph).foreach { format =>
        val interp = interpreter()
        val profiler = new Profiler(format, DummyPath("root"))
        interp.evaluator.profiler = profiler

        interp.interpret(code, DummyPath("root", "main.jsonnet")) ==> Right(ujson.True)

        val boxes = profiler.collectResult().boxes
        assert(boxes.exists(_.id == BoxId("Num", "<unknown>", -1)))
      }
    }

    test("flame graph frames can have synthetic positions") {
      val pos = new Position(null, -1)
      val frame = Expr.Apply1(
        pos,
        Expr.Id(pos, "f"),
        Val.Null(pos),
        tailstrict = false
      )
      val profiler = new Profiler(ProfileOutputFormat.FlameGraph, DummyPath("root"))

      val saved = profiler.enter(frame)
      profiler.exit(saved)

      val boxes = profiler.collectResult().boxes
      assert(boxes.exists(_.id.fileName == "<unknown>"))
    }
  }
}
