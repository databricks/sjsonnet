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
      val file = pos.currentFile
      val (frameFile, frameLine) = if (file == null) {
        ("<cached>", pos.offset)
      } else {
        ev.prettyIndex(pos) match {
          case None              => (file.relativeToString(ev.wd) + " offset", pos.offset)
          case Some((line, col)) => (file.relativeToString(ev.wd) + ":" + line, col)
        }
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
      val rawFrames = if (namedFrames.nonEmpty) namedFrames else allFrames

      // Compute enclosing function scope for each frame by scanning from
      // outermost to innermost. Each frame's callee name tells us what function
      // was entered; builtin calls (std.*) are transparent and don't change scope.
      val scopeNames = new Array[String](rawFrames.length)
      var currentScope: String = rootName
      var si = rawFrames.length - 1
      while (si >= 0) {
        scopeNames(si) = currentScope
        val frameName = rawFrames(si).exprErrorString
        if (frameName != null && !isBuiltinName(frameName)) {
          currentScope = frameName
        }
        si -= 1
      }
      if (rawFrames.nonEmpty) {
        scopeNames(0) = Option(rawFrames(0).exprErrorString).getOrElse(rootName)
      }

      // Deduplicate consecutive same-scope frames, keeping the innermost (first)
      val frames = new java.util.ArrayList[(Frame, String)](rawFrames.length)
      si = 0
      while (si < rawFrames.length) {
        val name = scopeNames(si)
        if (frames.isEmpty || frames.get(frames.size - 1)._2 != name) {
          frames.add((rawFrames(si), name))
        }
        si += 1
      }

      var msg = err.getMessage
      var frameStart = 0
      // Collapse innermost frame name into message when flagged, hiding it from "at" lines
      if (frames.size > 0 && rawFrames(0).collapseIntoMessage) {
        msg = "[" + frames.get(0)._2 + "] " + msg
        frameStart = 1
      }

      sb.append(msg)
      var i = frameStart
      while (i < frames.size) {
        val (f, name) = frames.get(i)
        appendFrame(sb, f, name)
        i += 1
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

  private def isBuiltinName(name: String): Boolean =
    name.startsWith("std.") || name == "default" ||
    name == "object comprehension" || name == "array comprehension"

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
    val file = pos.currentFile
    if (file == null) return None
    importer.read(file, binaryData = false).map { s =>
      val splitted =
        s.getParserInput().prettyIndex(pos.offset).split(':')
      if (splitted.length != 2) {
        throw new Error("Invalid pretty index format")
      }
      (splitted(0).toInt, splitted(1).toInt)
    }
  }
}
