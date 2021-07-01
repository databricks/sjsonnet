package sjsonnet

import java.io.{PrintWriter, StringWriter}

import fastparse.Parsed
import sjsonnet.Expr.Params

import scala.collection.mutable
import scala.util.control.NonFatal

/**
  * Wraps all the machinery of evaluating Jsonnet source code, from parsing to
  * evaluation to materialization, into a convenient wrapper class.
  */
class Interpreter(extVars: Map[String, ujson.Value],
                  tlaVars: Map[String, ujson.Value],
                  wd: Path,
                  importer: Importer,
                  preserveOrder: Boolean = false,
                  strict: Boolean = false,
                  storePos: Position => Unit = null,
                  val parseCache: mutable.HashMap[(Path, String), Either[String, (Expr, FileScope)]] = new mutable.HashMap,
                  ) { self =>

  val resolver = new CachedResolver(importer, parseCache) {
    override def process(expr: Expr, fs: FileScope): Either[String, (Expr, FileScope)] =
      Right(((new StaticOptimizer(evaluator)).optimize(expr), fs))
  }

  def createEvaluator(resolver: CachedResolver, extVars: Map[String, ujson.Value], wd: Path,
                      preserveOrder: Boolean, strict: Boolean): Evaluator =
    new Evaluator(resolver, extVars, wd, preserveOrder, strict)

  val evaluator: Evaluator = createEvaluator(resolver, extVars, wd, preserveOrder, strict)

  def interpret(txt: String, path: Path): Either[String, ujson.Value] = {
    interpret0(txt, path, ujson.Value)
  }
  def interpret0[T](txt: String,
                    path: Path,
                    visitor: upickle.core.Visitor[T, T]): Either[String, T] = {
    for{
      v <- evaluate(txt, path)
      r <- materialize(v, visitor)
    } yield r
  }

  def evaluate[T](txt: String, path: Path): Either[String, Val] = {
    resolver.cache(path) = txt
    for{
      res <- resolver.parse(path, txt) match {
        case Left(msg) => Left("Parse error: " + msg)
        case r => r
      }
      (parsed, fs) = res
      res0 <-
        try Right(evaluator.visitExpr(parsed)(ValScope.empty))
        catch{case NonFatal(e) =>
          val s = new StringWriter()
          val p = new PrintWriter(s)
          e.printStackTrace(p)
          p.close()
          Left(s.toString.replace("\t", "    "))
        }
      res = res0 match{
        case f: Val.Func =>
          val defaults2 = f.params.defaultExprs.clone()
          var i = 0
          while(i < defaults2.length) {
            tlaVars.get(f.params.names(i)) match {
              case Some(v) => defaults2(i) = Materializer.toExpr(v)(evaluator)
              case None =>
            }
            i += 1
          }
          new Val.Func(f.pos, f.defSiteValScope, Params(f.params.names, defaults2)) {
            def evalRhs(vs: ValScope, es: EvalScope, fs: FileScope, pos: Position) = f.evalRhs(vs, es, fs, pos)
            override def evalDefault(expr: Expr, vs: ValScope, es: EvalScope) = f.evalDefault(expr, vs, es)
          }
        case x => x
      }
    } yield res
  }

  def materialize[T](res: Val, visitor: upickle.core.Visitor[T, T]): Either[String, T] = {
    val m =
      if(storePos == null) Materializer
      else new Materializer {
        override def storePos(pos: Position): Unit = self.storePos(pos)
        override def storePos(v: Val): Unit = {
          storePos(
            v match {
              case v: Val.Obj if v.hasKeys => v.pos
              case v: Val.Arr if v.length > 0 => v.pos
              case _ => null
            }
          )
        }
      }
    try Right(m.apply0(res, visitor)(evaluator))
    catch{
      case Error.Delegate(msg) => Left(msg)
      case NonFatal(e) =>
        val s = new StringWriter()
        val p = new PrintWriter(s)
        e.printStackTrace(p)
        p.close()
        Left(s.toString.replace("\t", "    "))
    }
  }
}
