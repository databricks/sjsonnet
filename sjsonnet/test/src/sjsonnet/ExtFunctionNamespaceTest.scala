package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.functions.FunctionModule
import utest._

object ExtFunctionNamespaceTest extends TestSuite with FunctionModule {
  override final val name: String = "ext"
  override final lazy val module: Val.Obj = moduleFromFunctions(extFunctions: _*)

  private val extFunctions: Seq[(String, Val.Func)] = Seq(
    builtin("objectReplaceKey", "obj", "key", "newKey") {
      (pos, ev, o: Val.Obj, key: String, newKey: String) =>
        val bindings: Array[(String, Val.Obj.Member)] = for {
          k <- o.visibleKeyNames
          v = o.value(k, pos.fileScope.noOffsetPos)(ev)
        } yield {
          val newKeyName = if (k == key) newKey else k
          (newKeyName, new Val.Obj.ConstMember(false, Visibility.Normal, v))
        }
        Val.Obj.mk(pos, bindings)
    }
  )

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

  def check(s: String, expected: ujson.Value): Unit =
    interpreter.interpret(s, DummyPath("(memory)")) ==> Right(expected)

  def tests: Tests = Tests {
    "test objectReplaceKey in ext namespace" - {
      check(
        s"""
           |local obj = {"a": 1, "b": 2};
           |
           |ext.objectReplaceKey(obj, "a", "c")
           |""".stripMargin,
        ujson.Obj("c" -> 1, "b" -> 2)
      )
    }
  }
}
