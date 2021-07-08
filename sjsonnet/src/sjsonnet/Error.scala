package sjsonnet

import fastparse.IndexedParserInput

import scala.util.control.NonFatal

/**
  * An exception that can keep track of the Sjsonnet call-stack while it is
  * propagating upwards. This helps provide good error messages with line
  * numbers pointing towards user code.
  */
class Error(val msg: String, stack: List[StackTraceElement] = Nil, underlying: Option[Throwable] = None)
    extends Exception(msg, underlying.orNull) {

  setStackTrace(stack.toArray.reverse)

  override def fillInStackTrace: Throwable = this

  def addFrame(pos: Position, wd: Path, expr: Expr = null)(implicit ev: EvalErrorScope): Error = {
    if(stack.isEmpty || alwaysAddPos(expr)) {
      val cl = ""
  //    val cl = expr match {
  //      case null => ""
  //      case expr => expr.getClass.getName.replaceFirst("^sjsonnet.Expr\\$(.*)", "[$1]")
  //    }
      val newFrame = ev.prettyIndex(pos) match {
        case None =>
          new StackTraceElement(
            cl, "",
            pos.currentFile.relativeToString(wd) + " offset:",
            pos.offset
          )
        case Some((line, col)) =>
          new StackTraceElement(
            cl, "",
            pos.currentFile.relativeToString(wd) + ":" + line,
            col.toInt
          )
      }
      stack match {
        case h :: _ if h == newFrame => this
        case _ => new Error(msg, newFrame :: stack, underlying)
      }
    } else this
  }

  private[this] def alwaysAddPos(expr: Expr): Boolean = expr match {
    case _: Expr.LocalExpr | _: Expr.Arr | _: Expr.ObjExtend | _: Expr.ObjBody | _: Expr.IfElse => false
    case _ => true
  }
}

object Error {
  def withStackFrame[T](expr: Expr)
                       (implicit evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(expr.pos, evaluator.wd, expr)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e)).addFrame(expr.pos, evaluator.wd, expr)
  }

  def fail(msg: String, pos: Position)
          (implicit evaluator: EvalErrorScope): Nothing =
    throw new Error(msg, Nil, None).addFrame(pos, evaluator.wd)

  def fail(msg: String): Nothing = throw new Error(msg)
}

trait EvalErrorScope {
  def extVars: Map[String, ujson.Value]
  def importer: CachedImporter
  def wd: Path

  def prettyIndex(pos: Position): Option[(Int, Int)] = {
    importer.read(pos.currentFile).map { s =>
      val Array(line, col) =
        new IndexedParserInput(s).prettyIndex(pos.offset).split(':')
      (line.toInt, col.toInt)
    }
  }
}
