package sjsonnet

import ujson.Value

object TestUtils {
  def eval0(
      s: String,
      preserveOrder: Boolean = false,
      strict: Boolean = false,
      brokenAssertionLogic: Boolean = false,
      maxStack: Int = 500,
      std: sjsonnet.stdlib.StdLibModule = sjsonnet.stdlib.StdLibModule.Default,
      strictFormatBooleanConversions: Boolean = false)
      : Either[String, Value] = {
    new Interpreter(
      Map(),
      Map(),
      DummyPath(),
      Importer.empty,
      parseCache = new DefaultParseCache,
      new Settings(
        preserveOrder = preserveOrder,
        strict = strict,
        strictFormatBooleanConversions = strictFormatBooleanConversions,
        throwErrorForInvalidSets = true,
        brokenAssertionLogic = brokenAssertionLogic,
        maxStack = maxStack
      ),
      std = std.module
    ).interpret(s, DummyPath("(memory)"))
  }

  def eval(
      s: String,
      preserveOrder: Boolean = false,
      strict: Boolean = false,
      brokenAssertionLogic: Boolean = false,
      maxStack: Int = 500,
      std: sjsonnet.stdlib.StdLibModule = sjsonnet.stdlib.StdLibModule.Default,
      strictFormatBooleanConversions: Boolean = false): Value = {
    eval0(
      s,
      preserveOrder,
      strict,
      brokenAssertionLogic,
      maxStack,
      std,
      strictFormatBooleanConversions
    ) match {
      case Right(x) => x
      case Left(e)  => throw new Exception(e)
    }
  }

  def evalErr(
      s: String,
      preserveOrder: Boolean = false,
      strict: Boolean = false,
      brokenAssertionLogic: Boolean = false,
      maxStack: Int = 500,
      std: sjsonnet.stdlib.StdLibModule = sjsonnet.stdlib.StdLibModule.Default,
      strictFormatBooleanConversions: Boolean = false): String = {
    eval0(
      s,
      preserveOrder,
      strict,
      brokenAssertionLogic,
      maxStack,
      std,
      strictFormatBooleanConversions
    ) match {
      case Left(err) =>
        err.split('\n').map(_.trim).mkString("\n") // normalize inconsistent indenation on JVM vs JS
      case Right(r) => throw new Exception(s"Expected exception, got result: $r")
    }
  }
}
