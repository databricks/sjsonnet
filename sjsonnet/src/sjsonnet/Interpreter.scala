package sjsonnet

import java.io.{PrintWriter, StringWriter}

import scala.collection.mutable

import sjsonnet.Expr.Params

import scala.util.control.NonFatal

/**
  * Wraps all the machinery of evaluating Jsonnet source code, from parsing to
  * evaluation to materialization, into a convenient wrapper class.
  */
class Interpreter(queryExtVar: String => Option[ExternalVariable[_]],
                  queryTlaVar: String => Option[ExternalVariable[_]],
                  wd: Path,
                  importer: Importer,
                  parseCache: ParseCache,
                  settings: Settings,
                  storePos: Position => Unit,
                  warnLogger: String => Unit,
                  std: Val.Obj,
                  variableResolver: String => Option[Expr]
                 ) { self =>

  def this(extVars: Map[String, String],
           tlaVars: Map[String, String],
           wd: Path,
           importer: Importer,
           parseCache: ParseCache,
           settings: Settings = Settings.default,
           storePos: Position => Unit = null,
           warnLogger: (String => Unit) = null,
           std: Val.Obj = new Std().Std,
           variableResolver: String => Option[Expr] = _ => None) =
    this(key => extVars.get(key).map(ExternalVariable.code(_)),
      key => tlaVars.get(key).map(ExternalVariable.code(_)),
      wd, importer, parseCache, settings, storePos, warnLogger, std, variableResolver)

  private val noOffsetPos = new Position(new FileScope(wd), -1)

  protected val internedStrings = new mutable.HashMap[String, String]

  protected val internedStaticFieldSets = new mutable.HashMap[Val.StaticObjectFieldSet, java.util.LinkedHashMap[String, java.lang.Boolean]]

  val resolver = createResolver(parseCache)

  val varResolver = createResolver(createVarParseCache)

  private def createResolver(parseCache: ParseCache) = new CachedResolver(importer, parseCache, settings.strictImportSyntax, internedStrings, internedStaticFieldSets) {
    override def process(expr: Expr, fs: FileScope): Either[Error, (Expr, FileScope)] = {
      handleException(createOptimizer(evaluator, std, internedStrings, internedStaticFieldSets).optimize(expr), fs)
    }
  }

  protected def createOptimizer(ev: EvalScope,
                                std: Val.Obj,
                                internedStrings: mutable.HashMap[String, String],
                                internedStaticFieldSets: mutable.HashMap[Val.StaticObjectFieldSet, java.util.LinkedHashMap[String, java.lang.Boolean]]): StaticOptimizer = {
    new StaticOptimizer(ev, variableResolver, std, internedStrings, internedStaticFieldSets)
  }

  /**
   * A cache for parsing variables.
   * */
  def createVarParseCache: ParseCache = new DefaultParseCache()

  private def warn(e: Error): Unit = warnLogger("[warning] " + formatError(e))

  def createEvaluator(resolver: CachedResolver, extVars: String => Option[Expr], wd: Path,
                      settings: Settings, warn: Error => Unit): Evaluator = if (settings.useNewEvaluator)
    new NewEvaluator(resolver, extVars, wd, settings, warn)
  else
    new Evaluator(resolver, extVars, wd, settings, warn)

  /**
   * Evaluate a variable to an `Expr`.
   * */
  def evaluateVar(ctx: String, k: String, externalVariable: ExternalVariable[_]): Expr = externalVariable match {
    case ExternalVariable(ExternalVariableKind.Code, v: String) => parseVar(s"$ctx-var $k", v)
    case ExternalVariable(ExternalVariableKind.Expr, v: Expr) => v
    case ExternalVariable(ExternalVariableKind.Variable, v: String) => Val.Str(noOffsetPos, v)
    case _ => throw new Error(s"Unsupported external variable kind: ${externalVariable.kind}", Nil, None)
  }

  /**
   * Parse a variable to an `Expr`.
   * */
  def parseVar(k: String, v: String): Expr = {
    varResolver.parse(wd / Util.wrapInLessThanGreaterThan(k), StaticResolvedFile(v))(evaluator).fold(throw _, _._1)
  }

  lazy val evaluator: Evaluator = createEvaluator(
    resolver,
    // parse extVars lazily, because they can refer to each other and be recursive
    k => queryExtVar(k).map(v => evaluateVar("ext", k, v)),
    wd,
    settings,
    warn
  )

  evaluator // force the lazy val

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

  def evaluate(txt: String, path: Path): Either[Error, Val] = {
    val resolvedImport = StaticResolvedFile(txt)
    resolver.cache(path) = resolvedImport
    for{
      res <- resolver.parse(path, resolvedImport)(evaluator)
      (parsed, _) = res
      res0 <- handleException(evaluator.visitExpr(parsed)(ValScope.empty))
      res = res0 match{
        case f: Val.Func =>
          val defaults2 = f.params.defaultExprs.clone()
          val tlaExpressions = collection.mutable.Set.empty[Expr]
          var i = 0
          while(i < defaults2.length) {
            val k = f.params.names(i)
            for(v <- queryTlaVar(k)){
              val parsed = evaluateVar("tla", k, v)
              defaults2(i) = parsed
              tlaExpressions.add(parsed)
            }
            i += 1
          }
          new Val.Func(f.pos, f.defSiteValScope, Params(f.params.names, defaults2)) {
            def evalRhs(vs: ValScope, es: EvalScope, fs: FileScope, pos: Position) = f.evalRhs(vs, es, fs, pos)
            override def evalDefault(expr: Expr, vs: ValScope, es: EvalScope) = {
              evaluator.visitExpr(expr)(if (tlaExpressions.exists(_ eq expr)) ValScope.empty else vs)
            }
          }.apply0(f.pos)(evaluator)
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
