package sjsonnet

import java.io.{PrintWriter, StringWriter}

import ammonite.ops.{Path, read}
import fastparse.core.Parsed

class Interpreter(parser: Parser, scope: Scope) {
  val evaluator = new Evaluator(parser, scope)
  def interpret(p: Path): Either[String, ujson.Js] = {
    for{
      txt <- try Right(read(p)) catch{ case e: Throwable => Left(e.toString) }
      json <- interpret(txt)
    } yield json
  }
  def interpret(txt: String): Either[String, ujson.Js] = {
    for{
      parsed <- parser.expr.parse(txt) match{
        case f @ Parsed.Failure(l, i, e) => Left("Parse Error: expected " + f.msg)
        case Parsed.Success(r, index) => Right(r)
      }
      res <-
        try Right(evaluator.visitExpr(parsed, scope))
        catch{case e: Throwable =>
          val s = new StringWriter()
          val p = new PrintWriter(s)
          e.printStackTrace(p)
          p.close()
          Left(s.toString.replace("\t", "    "))
        }
      json <-
        try Right(Materializer(res))
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
