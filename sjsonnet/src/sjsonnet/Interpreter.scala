package sjsonnet

import java.io.{PrintWriter, StringWriter}

import fastparse.Parsed
import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.{FieldName, Member, ObjBody, Params}

class Interpreter(parseCache: collection.mutable.Map[String, fastparse.Parsed[Expr]],
                  scope: Scope,
                  extVars: Map[String, ujson.Value],
                  tlaVars: Map[String, ujson.Value],
                  wd: Path,
                  importer: (Scope, String) => Option[(Path, String)]) {
  val evaluator = new Evaluator(
    parseCache,
    scope,
    extVars,
    wd,
    importer
  )

  def interpret(p: Path): Either[String, ujson.Value] = {
    for{
      txt <- p.read().toRight(s"Failed to read file $p")
      json <- interpret(txt)
    } yield json
  }

  def interpret(txt: String): Either[String, ujson.Value] = {
    for{
      parsed <- parseCache.getOrElseUpdate(txt, fastparse.parse(txt, Parser.document(_))) match{
        case f @ Parsed.Failure(l, i, e) => Left("Parse error: " + f.trace().msg)
        case Parsed.Success(r, index) => Right(r)
      }
      res0 <-
        try Right(evaluator.visitExpr(parsed, scope))
        catch{case e: Throwable =>
          val s = new StringWriter()
          val p = new PrintWriter(s)
          e.printStackTrace(p)
          p.close()
          Left(s.toString.replace("\t", "    "))
        }
      res = res0 match{
        case f: Val.Func =>
          f.copy(params = Params(f.params.args.map{ case (k, default) =>
            (k, tlaVars.get(k) match{
              case None => default
              case Some(v) => Some(Materializer.toExpr(v))
            })
          }))
        case x => x
      }
      json <-
        try Right(Materializer(res, extVars, wd))
        catch{
          case DelegateError(msg) => Left(msg)
          case e: Throwable =>
            val s = new StringWriter()
            val p = new PrintWriter(s)
            e.printStackTrace(p)
            p.close()
            Left(s.toString.replace("\t", "    "))
        }
    } yield json
  }
}
