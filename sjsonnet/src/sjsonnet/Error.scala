package sjsonnet

import java.io.{PrintWriter, StringWriter}

/**
 * Resolved stack frame captured at error-throw time. Positions are pre-resolved to file:line:col so
 * that rendering does not need access to the evaluator.
 */
final class StackTrace(val name: String, val file: String, val line: Int, val col: Int)

/**
 * An exception carrying a Jsonnet-level stack trace. The trace is captured from the evaluator's
 * live call stack when the error is thrown (via [[Error.fail]]).
 */
class Error(
    msg: String,
    private[sjsonnet] val trace: Array[StackTrace] = Array.empty,
    underlying: Option[Throwable] = None)
    extends Exception(msg, underlying.orNull) {

  override def fillInStackTrace: Throwable = this
}

object Error {
  def fail(msg: String, pos: Position)(implicit ev: EvalErrorScope): Nothing =
    throw new Error(msg, ev.captureTrace(pos))

  def fail(msg: String, expr: Expr)(implicit ev: EvalErrorScope): Nothing =
    fail(msg, expr.pos)

  def fail(msg: String): Nothing =
    throw new Error(msg)

  private def errorPrefix(err: Error): String = err match {
    case _: ParseError  => "sjsonnet.ParseError: "
    case _: StaticError => "sjsonnet.StaticError: "
    case _              => "sjsonnet.Error: "
  }

  def formatError(e: Throwable): String = e match {
    case err: Error if err.trace.nonEmpty =>
      val sb = new StringBuilder
      sb.append(errorPrefix(err)).append(err.getMessage)
      for (frame <- err.trace) {
        sb.append("\n    at [").append(frame.name).append(']')
        if (frame.file != null) {
          sb.append(".(")
            .append(frame.file)
            .append(':')
            .append(frame.line)
            .append(':')
            .append(frame.col)
            .append(')')
        }
      }
      sb.append('\n')
      sb.toString
    case err: Error =>
      errorPrefix(err) + err.getMessage + '\n'
    case _ =>
      val s = new StringWriter()
      val p = new PrintWriter(s)
      try { e.printStackTrace(p); s.toString.replace("\t", "    ") }
      finally p.close()
  }
}

class ParseError(
    msg: String,
    _trace: Array[StackTrace] = Array.empty,
    underlying: Option[Throwable] = None,
    val offset: Int = -1)
    extends Error(msg, _trace, underlying)

class StaticError(
    msg: String,
    _trace: Array[StackTrace] = Array.empty,
    underlying: Option[Throwable] = None)
    extends Error(msg, _trace, underlying)

object StaticError {
  def fail(msg: String, expr: Expr)(implicit ev: EvalErrorScope): Nothing = {
    var trace = ev.captureTrace(expr.pos)
    if (trace.isEmpty && expr.pos != null) {
      ev.prettyIndex(expr.pos) match {
        case Some((line, col)) =>
          trace = Array(
            new StackTrace(
              Util.wrapInLessThanGreaterThan("root"),
              expr.pos.currentFile.relativeToString(ev.wd),
              line,
              col
            )
          )
        case None =>
      }
    }
    throw new StaticError(msg, trace)
  }
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

  def captureTrace(throwPos: Position): Array[StackTrace] = Array.empty
}
