package sjsonnet

import java.io.{PrintWriter, StringWriter}
import scala.util.control.NonFatal

/**
 * An exception that can keep track of the Sjsonnet call-stack while it is propagating upwards. This
 * helps provide good error messages with line numbers pointing towards user code.
 */
class Error(msg: String, val stack: List[Error.Frame] = Nil, underlying: Option[Throwable] = None)
    extends Exception(msg, underlying.orNull) {

  setStackTrace(stack.reverseIterator.map(_.ste).toArray)

  override def fillInStackTrace: Throwable = this

  def addFrame(pos: Position, expr: Expr = null)(implicit ev: EvalErrorScope): Error = {
    if (stack.isEmpty) {
      val name = if (expr != null && isApplyOrBuiltin(expr)) expr.exprErrorString else null
      if (name != null)
        copy(stack = new Error.Frame(pos, name, collapseIntoMessage = true) :: Nil)
      else
        addFrameString(pos, name)
    } else if (expr != null && isApplyOrBuiltin(expr)) {
      addFrameString(pos, expr.exprErrorString)
    } else this
  }

  def addFrameString(pos: Position, exprErrorString: String)(implicit ev: EvalErrorScope): Error = {
    stack match {
      case s :: ss if s.exprErrorString == null && exprErrorString != null =>
        val collapse = s.pos == pos
        copy(stack = new Error.Frame(s.pos, exprErrorString, collapse) :: ss)
      case s :: ss if s.pos == pos =>
        if (s.exprErrorString == null && exprErrorString != null)
          copy(stack = new Error.Frame(pos, exprErrorString, collapseIntoMessage = true) :: ss)
        else this
      case _ => copy(stack = new Error.Frame(pos, exprErrorString) :: stack)
    }
  }

  def forceAddFrame(pos: Position, name: String)(implicit ev: EvalErrorScope): Error =
    copy(stack = new Error.Frame(pos, name) :: stack)

  def asSeenFrom(ev: EvalErrorScope): Error =
    copy(stack = stack.map(_.asSeenFrom(ev)))

  protected def copy(
      msg: String = msg,
      stack: List[Error.Frame] = stack,
      underlying: Option[Throwable] = underlying) =
    new Error(msg, stack, underlying)

  private def isApplyOrBuiltin(expr: Expr): Boolean = expr match {
    case _: Expr.Apply | _: Expr.Apply0 | _: Expr.Apply1 | _: Expr.Apply2 | _: Expr.Apply3 |
        _: Expr.ApplyBuiltin | _: Expr.ApplyBuiltin0 | _: Expr.ApplyBuiltin1 |
        _: Expr.ApplyBuiltin2 | _: Expr.ApplyBuiltin3 | _: Expr.ApplyBuiltin4 | _: Expr.Comp |
        _: Expr.ObjBody.ObjComp =>
      true
    case _ => false
  }
}

object Error {
  final class Frame(
      val pos: Position,
      val exprErrorString: String,
      val collapseIntoMessage: Boolean = false)(implicit ev: EvalErrorScope) {
    val ste: StackTraceElement = {
      val cl = if (exprErrorString == null) "" else s"[$exprErrorString]"
      val (frameFile, frameLine) = ev.prettyIndex(pos) match {
        case None              => (pos.currentFile.relativeToString(ev.wd) + " offset", pos.offset)
        case Some((line, col)) => (pos.currentFile.relativeToString(ev.wd) + ":" + line, col)
      }
      new StackTraceElement(cl, "", frameFile, frameLine)
    }

    def asSeenFrom(ev: EvalErrorScope): Frame =
      if (ev eq this.ev) this
      else new Frame(pos, exprErrorString, collapseIntoMessage)(ev)
  }

  def withStackFrame[T](expr: Expr)(implicit
      evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error    => throw e.addFrame(expr.pos, expr)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e)).addFrame(expr.pos, expr)
  }

  def fail(msg: String, expr: Expr)(implicit ev: EvalErrorScope): Nothing =
    fail(msg, expr.pos)

  def fail(msg: String, pos: Position, cl: String = null)(implicit ev: EvalErrorScope): Nothing =
    throw new Error(msg, new Frame(pos, cl) :: Nil, None)

  def fail(msg: String): Nothing =
    throw new Error(msg)

  private val rootName = Util.wrapInLessThanGreaterThan("root")

  def formatError(e: Throwable): String = e match {
    case err: Error if err.stack.nonEmpty =>
      val sb = new StringBuilder
      sb.append(err.getClass.getName).append(": ")

      val allFrames = err.stack.reverse
      val namedFrames = allFrames.filter(_.exprErrorString != null)
      val frames = if (namedFrames.nonEmpty) namedFrames else allFrames

      var msg = err.getMessage
      var frameStart = 0
      while (
        frameStart < frames.length - 1 &&
        (frames(frameStart).collapseIntoMessage ||
        frames(frameStart).pos == frames(frameStart + 1).pos)
      ) {
        val name = Option(frames(frameStart).exprErrorString).getOrElse(rootName)
        msg = "[" + name + "] " + msg
        frameStart += 1
      }

      if (frameStart < frames.length && frames(frameStart).collapseIntoMessage) {
        val f = frames(frameStart)
        val name = Option(f.exprErrorString).getOrElse(rootName)
        msg = "[" + name + "] " + msg
        sb.append(msg)
        appendFrame(sb, f, rootName)
      } else {
        sb.append(msg)
        var i = frameStart
        while (i < frames.length) {
          val f = frames(i)
          val name =
            if (i == frames.length - 1) {
              val n = f.exprErrorString
              if (n == null || n == rootName) rootName else n
            } else
              Option(f.exprErrorString).getOrElse(rootName)
          appendFrame(sb, f, name)
          i += 1
        }
      }
      if (err.getCause != null) {
        sb.append("\nCaused by: ")
        val s = new StringWriter()
        val p = new PrintWriter(s)
        try {
          err.getCause.printStackTrace(p)
          sb.append(s.toString.replace("\t", "    "))
        } finally {
          p.close()
        }
      } else {
        sb.append('\n')
      }
      sb.toString

    case _ =>
      val s = new StringWriter()
      val p = new PrintWriter(s)
      try {
        e.printStackTrace(p)
        s.toString.replace("\t", "    ")
      } finally {
        p.close()
      }
  }

  private def appendFrame(sb: StringBuilder, f: Frame, name: String): Unit = {
    sb.append("\n    at [").append(name).append("]")
    if (f.pos != null) {
      sb.append(".(")
      sb.append(f.ste.getFileName).append(':').append(f.ste.getLineNumber)
      sb.append(')')
    }
  }
}

class ParseError(
    msg: String,
    override val stack: List[Error.Frame] = Nil,
    underlying: Option[Throwable] = None,
    val offset: Int = -1)
    extends Error(msg, stack, underlying) {

  override protected def copy(
      msg: String = msg,
      stack: List[Error.Frame] = stack,
      underlying: Option[Throwable] = underlying): sjsonnet.ParseError =
    new ParseError(msg, stack, underlying, offset)
}

class StaticError(
    msg: String,
    override val stack: List[Error.Frame] = Nil,
    underlying: Option[Throwable] = None)
    extends Error(msg, stack, underlying) {

  override protected def copy(
      msg: String = msg,
      stack: List[Error.Frame] = stack,
      underlying: Option[Throwable] = underlying): sjsonnet.StaticError =
    new StaticError(msg, stack, underlying)
}

object StaticError {
  def fail(msg: String, expr: Expr)(implicit ev: EvalErrorScope): Nothing =
    throw new StaticError(msg, new Error.Frame(expr.pos, null) :: Nil, None)
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
