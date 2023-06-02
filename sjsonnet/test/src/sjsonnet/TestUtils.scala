package sjsonnet

object TestUtils {
  def eval0(s: String, preserveOrder: Boolean = false, strict: Boolean = false) = {
    new Interpreter(
      Map(),
      Map(),
      DummyPath(),
      Importer.empty,
      parseCache = new DefaultParseCache,
      new Settings(preserveOrder = preserveOrder, strict = strict)
    ).interpret(s, DummyPath("(memory)"))
  }

  def eval(s: String, preserveOrder: Boolean = false, strict: Boolean = false) = {
    eval0(s, preserveOrder, strict) match {
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }

  def evalErr(s: String, preserveOrder: Boolean = false, strict: Boolean = false) = {
    eval0(s, preserveOrder, strict) match{
      case Left(err) => err.split('\n').map(_.trim).mkString("\n")  // normalize inconsistent indenation on JVM vs JS
      case Right(r) => throw new Exception(s"Expected exception, got result: $r")
    }
  }
}
