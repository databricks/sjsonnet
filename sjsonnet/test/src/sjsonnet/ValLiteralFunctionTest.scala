package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.functions.FunctionBuilder
import utest._

import java.{lang, util}
import scala.collection.mutable

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

  private val interpreter = new Interpreter(
    Map(),
    Map(),
    DummyPath(),
    Importer.empty,
    parseCache = new DefaultParseCache
  ) {
    override protected def createOptimizer(ev: EvalScope, std: Val.Obj, internedStrings: mutable.HashMap[String, String], internedStaticFieldSets: mutable.HashMap[Val.StaticObjectFieldSet, util.LinkedHashMap[String, lang.Boolean]]): StaticOptimizer = {
      new StaticOptimizer(ev, std, internedStrings, internedStaticFieldSets) {
        override def variableResolver(name: String): Expr = {
          if (name == "$ext" || name == "ext") {
            ext
          } else {
            null
          }
        }
      }
    }
  }

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
