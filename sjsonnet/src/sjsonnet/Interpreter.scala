package sjsonnet

import java.io.{PrintWriter, StringWriter}

import fastparse.Parsed
import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.{FieldName, Member, ObjBody, Params}

class Interpreter(parseCache: collection.mutable.Map[String, fastparse.Parsed[Expr]],
                  scope: Scope,
                  extVars: Map[String, ujson.Js],
                  tlaVars: Map[String, ujson.Js],
                  wd: os.Path,
                  allowedInputs: Option[Set[os.Path]],
                  importer: Option[(Scope, String) => Option[os.Path]] = None) {
  val evaluator = new Evaluator(parseCache, scope, extVars, wd, allowedInputs, importer)
  def interpret(p: os.Path): Either[String, ujson.Js] = {
    val txt: Either[String, String] = try Right(os.read(p)) catch{ case e: Throwable => Left(e.toString) }
    txt.right.flatMap(interpret)
  }

  def interpret(txt: String): Either[String, ujson.Js] = {
    val parsed: Either[String, Expr] = parseCache.getOrElseUpdate(txt, fastparse.parse(txt, Parser.document(_))) match{
      case f @ Parsed.Failure(l, i, e) => Left("Parse error: " + f.trace().msg)
      case Parsed.Success(r, index) => Right(r)
    }

    val res0: Either[String, Val] = parsed.right.flatMap { result =>
      try Right(evaluator.visitExpr(result, scope))
      catch {
        case e: Throwable =>
          val s = new StringWriter()
          val p = new PrintWriter(s)
          e.printStackTrace(p)
          p.close()
          Left(s.toString.replace("\t", "    "))
      }
    }

    val res: Either[String, Val] = res0.right.map {
      case f: Val.Func =>
        f.copy(params = Params(f.params.args.map{ case (k, default) =>
          (k, tlaVars.get(k) match{
            case None => default
            case Some(v) => Some(Materializer.toExpr(v))
          })
        }))
      case x => x
    }

    val json: Either[String, ujson.Js] = res.right.flatMap { result =>
      try Right(Materializer(result, extVars, wd))
      catch{
        case DelegateError(msg) => Left(msg)
        case e: Throwable =>
          val s = new StringWriter()
          val p = new PrintWriter(s)
          e.printStackTrace(p)
          p.close()
          Left(s.toString.replace("\t", "    "))
      }
    }

    json
  }
}
