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

  def addFrame(pos: Position, expr: Expr = null)(implicit ev: EvalErrorScope): Error = {
    if(stack.isEmpty || alwaysAddPos(expr)) {
      val exprErrorString = if(expr == null) null else expr.exprErrorString
      val newFrame = Error.mkFrame(pos, exprErrorString)
      stack match {
        case s :: ss if eq(s, newFrame) =>
          if(s.getClassName == "" && newFrame.getClassName != null) new Error(msg, newFrame :: ss, underlying)
          else this
        case _ => new Error(msg, newFrame :: stack, underlying)
      }
    } else this
  }

  private[this] def eq(s1: StackTraceElement, s2: StackTraceElement): Boolean =
    s1.getFileName == s2.getFileName && s1.getLineNumber == s2.getLineNumber

  private[this] def alwaysAddPos(expr: Expr): Boolean = expr match {
    case _: Expr.LocalExpr | _: Expr.Arr | _: Expr.ObjExtend | _: Expr.ObjBody | _: Expr.IfElse => false
    case _ => true
  }
}

object Error {
  def withStackFrame[T](expr: Expr)
                       (implicit evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(expr.pos, expr)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e)).addFrame(expr.pos, expr)
  }

  def fail(msg: String, expr: Expr)(implicit evaluator: EvalErrorScope): Nothing =
    throw new Error(msg, Nil, None).addFrame(expr.pos, expr)

  def fail(msg: String, pos: Position, cl: String = null)(implicit evaluator: EvalErrorScope): Nothing =
    throw new Error(msg, mkFrame(pos, cl) :: Nil, None)

  def fail(msg: String): Nothing =
    throw new Error(msg)

  private def mkFrame(pos: Position, exprErrorString: String)(implicit ev: EvalErrorScope): StackTraceElement = {
    val cl = if(exprErrorString == null) "" else s"[${exprErrorString}]"
    val (frameFile, frameLine) = ev.prettyIndex(pos) match {
      case None => (pos.currentFile.relativeToString(ev.wd) + " offset:", pos.offset)
      case Some((line, col)) => (pos.currentFile.relativeToString(ev.wd) + ":" + line, col.toInt)
    }
    new StackTraceElement(cl, "", frameFile, frameLine)
  }
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
