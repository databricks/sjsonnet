package sjsonnet

import ujson.Value

object TestUtils {
  def eval0(s: String,
            preserveOrder: Boolean = false,
            strict: Boolean = false,
            noDuplicateKeysInComprehension: Boolean = false,
            strictInheritedAssertions: Boolean = false,
            strictSetOperations: Boolean = true,
            disableBuiltinSpecialization: Boolean = false): Either[String, Value] = {
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
        strictSetOperations = strictSetOperations,
        disableBuiltinSpecialization = disableBuiltinSpecialization,
        throwErrorForInvalidSets = true
      )
    ).interpret(s, DummyPath("(memory)"))
  }

  def eval(s: String,
           preserveOrder: Boolean = false,
           strict: Boolean = false,
           noDuplicateKeysInComprehension: Boolean = false,
           strictInheritedAssertions: Boolean = false,
           strictSetOperations: Boolean = true,
           disableBuiltinSpecialization: Boolean = false): Value = {
    eval0(s, preserveOrder, strict, noDuplicateKeysInComprehension, strictInheritedAssertions, strictSetOperations, disableBuiltinSpecialization) match {
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }

  def evalErr(s: String,
              preserveOrder: Boolean = false,
              strict: Boolean = false,
              noDuplicateKeysInComprehension: Boolean = false,
              strictInheritedAssertions: Boolean = false,
              strictSetOperations: Boolean = true,
              disableBuiltinSpecialization: Boolean = false): String = {
    eval0(s, preserveOrder, strict, noDuplicateKeysInComprehension, strictInheritedAssertions, strictSetOperations, disableBuiltinSpecialization) match{
      case Left(err) => err.split('\n').map(_.trim).mkString("\n")  // normalize inconsistent indenation on JVM vs JS
      case Right(r) => throw new Exception(s"Expected exception, got result: $r")
    }
  }

  def assertSameEvalWithAndWithoutSpecialization(s: String): Unit = {
    val noSpecialization = eval(s, preserveOrder = true, disableBuiltinSpecialization = true)
    val withSpecialization = eval(s, preserveOrder = true, disableBuiltinSpecialization = false)
    // For better error messages, convert to string representation first
    val specializedStr = noSpecialization.toString()
    val unspecializedStr = withSpecialization.toString()

    assert(
      specializedStr == unspecializedStr,
      s"""Specialization mismatch for expression: $s
         |
         |Specialized result:   $specializedStr
         |Unspecialized result: $unspecializedStr
         |""".stripMargin
    )
  }
}
