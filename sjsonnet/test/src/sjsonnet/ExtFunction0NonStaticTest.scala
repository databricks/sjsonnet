package sjsonnet

import sjsonnet.functions.FunctionModule
import utest._

import java.{lang, util}
import scala.collection.mutable

object ExtFunction0NonStaticTest extends TestSuite with FunctionModule {
  override final val name: String = "ext"
  override final lazy val module: Val.Obj = moduleFromFunctions(extFunctions: _*)

  private object SayHello extends Val.Builtin0("sayHello") {
    override def evalRhs(ev: EvalScope, pos: Position): Val = {
      Val.Str(pos, "Hello, world Every time!")
    }
    override def staticSafe: Boolean = false
  }

  private val extFunctions: Seq[(String, Val.Func)] = Seq(builtin(SayHello))

  private val interpreter = new Interpreter(
    Map(),
    Map(),
    DummyPath(),
    Importer.empty,
    parseCache = new DefaultParseCache
  ) {
    override protected def createOptimizer(ev: EvalScope, std: Val.Obj, internedStrings: mutable.HashMap[String, String], internedStaticFieldSets: mutable.HashMap[Val.StaticObjectFieldSet, util.LinkedHashMap[String, lang.Boolean]]): StaticOptimizer =
      new StaticOptimizer(ev, std, internedStrings, internedStaticFieldSets) {
        override def variableResolver(name: String): Expr = {
          if (name == "$ext" || name == "ext") {
            module
          } else {
            null
          }
        }
      }
  }

  def check(s: String)(f: Function[Any, Boolean]): Unit = {
    val result = interpreter.interpret(s, DummyPath("(memory)"))
    assertMatch(result) {
      case Right(v) if f(v) => ()
      case Left(e) => throw new Exception(s"check failed: $s, $e")
    }
  }

  def tests: Tests = Tests {
    "test uuid function in ext namespace" - {
      check(
        s"""
           |local sayHello = ext.sayHello;
           |sayHello()
           |""".stripMargin) {
        case ujson.Str(v) => v == "Hello, world Every time!"
      }
    }
  }
}
