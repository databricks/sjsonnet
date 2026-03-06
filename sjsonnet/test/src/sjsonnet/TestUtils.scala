package sjsonnet

import ujson.Value

object TestUtils {
  def eval0(
      s: String,
      preserveOrder: Boolean = false,
      strict: Boolean = false,
      useNewEvaluator: Boolean = false,
      brokenAssertionLogic: Boolean = false,
      maxStack: Int = 500,
      aggressiveStaticOptimization: Boolean = false,
      std: sjsonnet.stdlib.StdLibModule = sjsonnet.stdlib.StdLibModule.Default)
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
        throwErrorForInvalidSets = true,
        useNewEvaluator = useNewEvaluator,
        brokenAssertionLogic = brokenAssertionLogic,
        maxStack = maxStack,
        aggressiveStaticOptimization = aggressiveStaticOptimization
      ),
      std = std.module
    ).interpret(s, DummyPath("(memory)"))
  }

  def eval(
      s: String,
      preserveOrder: Boolean = false,
      strict: Boolean = false,
      useNewEvaluator: Boolean = false,
      brokenAssertionLogic: Boolean = false,
      maxStack: Int = 500,
      aggressiveStaticOptimization: Boolean = false,
      std: sjsonnet.stdlib.StdLibModule = sjsonnet.stdlib.StdLibModule.Default): Value = {
    eval0(
      s,
      preserveOrder,
      strict,
      useNewEvaluator,
      brokenAssertionLogic,
      maxStack,
      aggressiveStaticOptimization,
      std
    ) match {
      case Right(x) => x
      case Left(e)  => throw new Exception(e)
    }
  }

  def evalErr(
      s: String,
      preserveOrder: Boolean = false,
      strict: Boolean = false,
      useNewEvaluator: Boolean = false,
      brokenAssertionLogic: Boolean = false,
      maxStack: Int = 500,
      aggressiveStaticOptimization: Boolean = false,
      std: sjsonnet.stdlib.StdLibModule = sjsonnet.stdlib.StdLibModule.Default): String = {
    eval0(
      s,
      preserveOrder,
      strict,
      useNewEvaluator,
      brokenAssertionLogic,
      maxStack,
      aggressiveStaticOptimization,
      std
    ) match {
      case Left(err) =>
        err.split('\n').map(_.trim).mkString("\n") // normalize inconsistent indenation on JVM vs JS
      case Right(r) => throw new Exception(s"Expected exception, got result: $r")
    }
  }
}
