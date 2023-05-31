package sjsonnet

import java.io.{PrintWriter, StringWriter}

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
                  val parseCache: ParseCache,
                  settings: Settings = Settings.default,
                  storePos: Position => Unit = null,
                  warnLogger: (String => Unit) = null,
                  std: Val.Obj = new Std().Std
                  ) { self =>

  val resolver = new CachedResolver(importer, parseCache) {
    override def process(expr: Expr, fs: FileScope): Either[Error, (Expr, FileScope)] =
      handleException(new StaticOptimizer(evaluator, std).optimize(expr), fs)
  }

  private def warn(e: Error): Unit = warnLogger("[warning] " + formatError(e))

  def createEvaluator(resolver: CachedResolver, extVars: Map[String, ujson.Value], wd: Path,
                      settings: Settings, warn: Error => Unit): Evaluator =
    new Evaluator(resolver, extVars, wd, settings, warn)

  val evaluator: Evaluator = createEvaluator(resolver, extVars, wd, settings, warn)

  def formatError(e: Error): String = {
    val s = new StringWriter()
    val p = new PrintWriter(s)
    e.printStackTrace(p)
    p.close()
    s.toString.replace("\t", "    ")
  }

  def interpret(txt: String, path: Path): Either[String, ujson.Value] =
    interpret0(txt, path, ujson.Value)

  def interpret0[T](txt: String,
                    path: Path,
                    visitor: upickle.core.Visitor[T, T]): Either[String, T] =
    (for{
      v <- evaluate(txt, path)
      r <- materialize(v, visitor)
    } yield r).left.map(formatError)

  private def handleException[T](f: => T): Either[Error, T] = {
    try Right(f) catch {
      case e: Error => Left(e)
      case NonFatal(e) =>
        Left(new Error("Internal error: "+e.toString, Nil, Some(e)))
    }
  }

  def evaluate[T](txt: String, path: Path): Either[Error, Val] = {
    resolver.cache(path) = txt
    for{
      res <- resolver.parse(path, txt)(evaluator)
      (parsed, _) = res
      res0 <- handleException(evaluator.visitExpr(parsed)(ValScope.empty))
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

  def materialize[T](res: Val, visitor: upickle.core.Visitor[T, T]): Either[Error, T] = {
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
    handleException(m.apply0(res, visitor)(evaluator))
  }
}
