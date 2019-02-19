package sjsonnet.api

import java.nio.file.Path

import fastparse.Parsed
import sjsonnet.Expr.Params
import sjsonnet._


object Interpreter {
  type Variables = Map[String, ujson.Js]
  val Variables: Map.type = Map

  def parse(text: String): Expr = {
    val result = fastparse.parse(text, Parser.document(_))
    result match{
      case f @ Parsed.Failure(_, _, _) => throw sjsonnet.Error("Parse error: " + f.trace().msg, Nil, None)
      case Parsed.Success(r, _) => r
    }
  }

  def parseVariables(s: String): Variables = {
    ujson.read(s).obj.toMap
  }

  def substitute(value: Val, vars: Variables): Val = {
    value match{
      case f: Val.Func =>
        f.copy(params = Params(f.params.args.map{ case (k, default) =>
          (k, vars.get(k) match{
            case None => default
            case Some(v) => Some(Materializer.toExpr(v))
          })
        }))
      case x => x
    }
  }

  def materialize(value: Val, extVars: Variables, args: Variables, root: Path): String = {
    val v = substitute(value, args)
    val wd = if (root == null) os.pwd else os.Path(root)
    val js = Materializer(v, extVars, wd)
    js.toString()
  }

  def evaluate(path: Path, root: Path): Val = {
    val p = os.Path(path)
    val text = os.read(p)
    val expr = parse(text)
    val parseCache = collection.mutable.Map[String, Parsed[Expr]]()
    val wd = if (root == null) os.pwd else os.Path(root)
    val scope = Scope.standard(p, wd, Nil)
    val extVars: Variables = Variables()
    val evaluator = new Evaluator(parseCache, scope, extVars, wd)
    evaluator.visitExpr(expr, scope)
  }
}
