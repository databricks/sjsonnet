package sjsonnet

import java.io.{PrintWriter, StringWriter}
import scala.util.control.{NoStackTrace, NonFatal}

/**
 * An exception that can keep track of the Sjsonnet call-stack while it is propagating upwards. This
 * helps provide good error messages with line numbers pointing towards user code.
 */
class Error(msg: String, stack: List[Error.Frame] = Nil, underlying: Option[Throwable] = None)
    extends Exception(msg, underlying.orNull)
    with NoStackTrace {

  setStackTrace(stack.reverseIterator.map(_.ste).toArray)

  override def fillInStackTrace: Throwable = this

  def addFrame(pos: Position, expr: Expr = null)(implicit ev: EvalErrorScope): Error = {
    if (stack.isEmpty || alwaysAddPos(expr)) {
      val exprErrorString = if (expr == null) null else expr.exprErrorString
      val newFrame = new Error.Frame(pos, exprErrorString)
      stack match {
        case s :: ss if s.pos == pos =>
          if (s.exprErrorString == null && exprErrorString != null) copy(stack = newFrame :: ss)
          else this
        case _ => copy(stack = newFrame :: stack)
      }
    } else this
  }

  def asSeenFrom(ev: EvalErrorScope): Error =
    copy(stack = stack.map(_.asSeenFrom(ev)))

  protected def copy(
      msg: String = msg,
      stack: List[Error.Frame] = stack,
      underlying: Option[Throwable] = underlying) =
    new Error(msg, stack, underlying)

  private def alwaysAddPos(expr: Expr): Boolean = expr match {
    case _: Expr.LocalExpr | _: Expr.Arr | _: Expr.ObjExtend | _: Expr.ObjBody | _: Expr.IfElse =>
      false
    case _ => true
  }
}

object Error {
  final class Frame(val pos: Position, val exprErrorString: String)(implicit ev: EvalErrorScope) {
    val ste: StackTraceElement = {
      val cl = if (exprErrorString == null) "" else s"[$exprErrorString]"
      val (frameFile, frameLine) = ev.prettyIndex(pos) match {
        case None              => (pos.currentFile.relativeToString(ev.wd) + " offset", pos.offset)
        case Some((line, col)) => (pos.currentFile.relativeToString(ev.wd) + ":" + line, col)
      }
      new StackTraceElement(cl, "", frameFile, frameLine)
    }

    def asSeenFrom(ev: EvalErrorScope): Frame =
      if (ev eq this.ev) this else new Frame(pos, exprErrorString)(ev)
  }

  def withStackFrame[T](expr: Expr)(implicit
      evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error    => throw e.addFrame(expr.pos, expr)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e)).addFrame(expr.pos, expr)
  }

  def fail(msg: String, expr: Expr)(implicit ev: EvalErrorScope): Nothing =
    fail(msg, expr.pos, expr.exprErrorString)

  def fail(msg: String, pos: Position, cl: String = null)(implicit ev: EvalErrorScope): Nothing =
    throw new Error(msg, new Frame(pos, cl) :: Nil, None)

  def fail(msg: String): Nothing =
    throw new Error(msg)

  def formatError(e: Throwable): String = {
    val s = new StringWriter()
    val p = new PrintWriter(s)
    try {
      e.printStackTrace(p)
      s.toString.replace("\t", "    ")
    } finally {
      p.close()
    }
  }
}

class ParseError(msg: String, stack: List[Error.Frame] = Nil, underlying: Option[Throwable] = None)
    extends Error(msg, stack, underlying) {

  override protected def copy(
      msg: String = msg,
      stack: List[Error.Frame] = stack,
      underlying: Option[Throwable] = underlying): sjsonnet.ParseError =
    new ParseError(msg, stack, underlying)
}

class StaticError(msg: String, stack: List[Error.Frame] = Nil, underlying: Option[Throwable] = None)
    extends Error(msg, stack, underlying) {

  override protected def copy(
      msg: String = msg,
      stack: List[Error.Frame] = stack,
      underlying: Option[Throwable] = underlying): sjsonnet.StaticError =
    new StaticError(msg, stack, underlying)
}

object StaticError {
  def fail(msg: String, expr: Expr)(implicit ev: EvalErrorScope): Nothing =
    throw new StaticError(msg, new Error.Frame(expr.pos, expr.exprErrorString) :: Nil, None)
}

trait EvalErrorScope {
  def extVars: String => Option[Expr]
  def importer: CachedImporter
  def wd: Path

  def prettyIndex(pos: Position): Option[(Int, Int)] = {
    importer.read(pos.currentFile, binaryData = false).map { s =>
      val splitted =
        s.getParserInput().prettyIndex(pos.offset).split(':')
      if (splitted.length != 2) {
        throw new Error("Invalid pretty index format")
      }
      (splitted(0).toInt, splitted(1).toInt)
    }
  }
}
