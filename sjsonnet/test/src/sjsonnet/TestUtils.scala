package sjsonnet

object TestUtils {
  def eval(s: String, preserveOrder: Boolean = false, strict: Boolean = false) = {
    new Interpreter(
      Map(),
      Map(),
      DummyPath(),
      Importer.empty,
      preserveOrder = preserveOrder,
      strict = strict
    ).interpret(s, DummyPath("(memory)")) match {
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }

}
