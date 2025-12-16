package sjsonnet

import upickle.core.Visitor
import utest._

object CustomValTests extends TestSuite {
  private final case class ImportantString(pos: Position, str: String, importance: Int)
      extends Val.Literal
      with Val.Custom {
    override def prettyName: String = "Important string"
    def materialize[T](visitor: Visitor[T, T])(implicit evaluator: EvalScope): T = {
      visitor.visitString(str + "!".repeat(importance), -1)
    }
  }

  private final class IncreaseImportance
      extends Val.Builtin1("increaseImportance", "important_string") {
    def evalRhs(arg1: Lazy, ev: EvalScope, pos: Position): Val = {
      arg1.force match {
        case importantString: ImportantString =>
          importantString.copy(importance = importantString.importance + 1)
        case other => other
      }
    }
  }

  private def variableResolve(name: String): Option[Expr] = {
    if (name == "message") {
      Some(ImportantString(new Position(null, 0), "message", 2))
    } else if (name == "increaseImportance") {
      Some(new IncreaseImportance().asFunc)
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
    test("test custom Val materialization") {
      check(s"""important""") {
        case ujson.Str(v) => v == "message!!"
        case _            => false
      }
    }

    test("test custom Val usage") {
      check(s"""increaseImportance(message)""") {
        case ujson.Str(v) => v == "message!!!"
        case _            => false
      }
    }
  }
}
