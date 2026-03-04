package sjsonnet

import sjsonnet.functions.FunctionModule
import utest._

object QualifiedNameBuilderTest extends TestSuite {

  /**
   * A custom TB module that overrides builtinQualifiedName to use "tb." prefix in error messages.
   */
  object TB extends FunctionModule {
    override final val name: String = "tb"

    override protected def builtinQualifiedName(name: String): String = s"tb.$name"

    override final lazy val module: Val.Obj = moduleFromFunctions(functions: _*)

    private val functions: Seq[(String, Val.Func)] = Seq(
      builtin("greet", "x") { (_: Position, _: EvalScope, x: String) =>
        s"Hello, $x!"
      },
      builtin[String, String]("failFunc", "x") { (_: Position, _: EvalScope, _: String) =>
        Error.fail("not implemented")
      }
    )
  }

  private def variableResolve(name: String): Option[Expr] = {
    if (name == "tb") Some(TB.module) else None
  }

  private val interpreter = new Interpreter(
    Map(),
    Map(),
    DummyPath(),
    Importer.empty,
    parseCache = new DefaultParseCache,
    variableResolver = variableResolve
  )

  def tests: Tests = Tests {
    test("TB module normal execution") {
      val result = interpreter.interpret("""tb.greet("world")""", DummyPath("(memory)"))
      result match {
        case Right(ujson.Str(v)) => assert(v == "Hello, world!")
        case other               => throw new Exception(s"Expected string result but got: $other")
      }
    }

    test("TB module error message uses tb prefix") {
      val result = interpreter.interpret("""tb.failFunc("hello")""", DummyPath("(memory)"))
      result match {
        case Left(err) =>
          assert(err.contains("tb.failFunc"))
          assert(!err.contains("std.failFunc"))
        case Right(v) =>
          throw new Exception(s"Expected error but got: $v")
      }
    }
  }
}
