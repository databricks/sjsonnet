package sjsonnet

object TestUtils {
  def eval0(s: String,
            preserveOrder: Boolean = false,
            strict: Boolean = false,
            noDuplicateKeysInComprehension: Boolean = false,
            strictInheritedAssertions: Boolean = false,
            strictSetOperations: Boolean = true) = {
    new Interpreter(
      Map(),
      Map(),
      DummyPath(),
      Importer.empty,
      parseCache = new DefaultParseCache,
      new Settings(
        preserveOrder = preserveOrder,
        strict = strict,
        noDuplicateKeysInComprehension = noDuplicateKeysInComprehension,
        strictInheritedAssertions = strictInheritedAssertions,
        strictSetOperations = strictSetOperations
      )
    ).interpret(s, DummyPath("(memory)"))
  }

  def eval(s: String,
           preserveOrder: Boolean = false,
           strict: Boolean = false,
           noDuplicateKeysInComprehension: Boolean = false,
           strictInheritedAssertions: Boolean = false,
           strictSetOperations: Boolean = true) = {
    eval0(s, preserveOrder, strict, noDuplicateKeysInComprehension, strictInheritedAssertions, strictSetOperations) match {
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }

  def evalErr(s: String,
              preserveOrder: Boolean = false,
              strict: Boolean = false,
              noDuplicateKeysInComprehension: Boolean = false,
              strictInheritedAssertions: Boolean = false,
              strictSetOperations: Boolean = true) = {
    eval0(s, preserveOrder, strict, noDuplicateKeysInComprehension, strictInheritedAssertions, strictSetOperations) match{
      case Left(err) => err.split('\n').map(_.trim).mkString("\n")  // normalize inconsistent indenation on JVM vs JS
      case Right(r) => throw new Exception(s"Expected exception, got result: $r")
    }
  }
}
