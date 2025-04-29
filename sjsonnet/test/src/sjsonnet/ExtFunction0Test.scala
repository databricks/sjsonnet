package sjsonnet

import sjsonnet.functions.FunctionModule
import utest._

object ExtFunction0Test extends TestSuite with FunctionModule {
  override final val name: String = "ext"
  override final lazy val module: Val.Obj = moduleFromFunctions(extFunctions: _*)

  private val extFunctions: Seq[(String, Val.Func)] = Seq(builtin("sayHello") { (_, _) =>
    "Hello, world!"
  })

  private def variableResolve(name: String): Option[Expr] = {
    if (name == "$ext" || name == "ext") {
      Some(module)
    } else {
      None
    }
  }

  private val interpreter = new Interpreter(
    Map(),
    Map(),
    DummyPath(),
    Importer.empty,
    parseCache = new DefaultParseCache,
    variableResolver = variableResolve
  )

  def check(s: String)(f: Function[Any, Boolean]): Unit = {
    val result = interpreter.interpret(s, DummyPath("(memory)"))
    assertMatch(result) {
      case Right(v) if f(v) => ()
      case Left(e)          => throw new Exception(s"check failed: $s, $e")
    }
  }

  def tests: Tests = Tests {
    "test uuid function in ext namespace" - {
      check(s"""
               |local sayHello = ext.sayHello;
               |sayHello()
               |""".stripMargin) {
        case ujson.Str(v) => v == "Hello, world!"
        case _            => false
      }
    }
  }
}
