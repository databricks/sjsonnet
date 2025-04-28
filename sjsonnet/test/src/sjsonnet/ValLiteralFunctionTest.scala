package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.functions.FunctionBuilder
import utest._

object ValLiteralFunctionTest extends TestSuite with FunctionBuilder {
  private val extFunctions: Map[String, Val.Func] = Map(
    builtin("identity", "obj") {
      (_, _, o: Val.Literal) =>
        o
    })

  private val ext = Val.Obj.mk(
    null,
    extFunctions.toSeq.map {
      case (k, v) => (k, new Val.Obj.ConstMember(false, Visibility.Hidden, v))
    }: _*
  )

  private def variableResolve(name: String): Option[Expr] = {
    if (name == "$ext" || name == "ext") {
      Some(ext)
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
    variableResolver = variableResolve,
  )

  def check(s: String, expected: ujson.Value): Unit =
    interpreter.interpret(s, DummyPath("(memory)")) ==> Right(expected)

  def tests: Tests = Tests {
    "test objectReplaceKey in ext namespace" - {
      check(
        s"""
           |local obj = {"a": 1, "b": 2};
           |
           |ext.identity(obj)
           |""".stripMargin, ujson.Obj("a" -> 1, "b" -> 2))
    }
  }
}
