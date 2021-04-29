package sjsonnet

import fastparse.IndexedParserInput

import scala.util.control.NonFatal

/**
  * An exception that can keep track of the Sjsonnet call-stack while it is
  * propagating upwards. This helps provide good error messages with line
  * numbers pointing towards user code.
  */
case class Error(msg: String,
                 stack: List[StackTraceElement],
                 underlying: Option[Throwable])
  extends Exception(msg, underlying.orNull){
  setStackTrace(stack.toArray.reverse)
  def addFrame(pos: Position, wd: Path)(implicit ev: EvalErrorScope) = {
    val newFrame = ev.prettyIndex(pos) match {
      case None =>
        new StackTraceElement(
          "", "",
          pos.currentFile.relativeToString(wd) + " offset:",
          pos.offset
        )
      case Some((line, col)) =>

        new StackTraceElement(
          "", "",
          pos.currentFile.relativeToString(wd) + ":" + line,
          col.toInt
        )
    }

    this.copy(stack = newFrame :: this.stack)
  }
}


object Error {
  def tryCatch[T](pos: Position)
                 (implicit evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e
    case e: Error.Delegate =>
      throw new Error(e.msg, Nil, None).addFrame(pos, evaluator.wd)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e)).addFrame(pos, evaluator.wd)
  }
  def tryCatchWrap[T](pos: Position)
                     (implicit evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(pos, evaluator.wd)
    case e: Error.Delegate =>
      throw new Error(e.msg, Nil, None).addFrame(pos, evaluator.wd)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e)).addFrame(pos, evaluator.wd)
  }
  def fail(msg: String, pos: Position)
          (implicit evaluator: EvalErrorScope) = {
    throw Error(msg, Nil, None).addFrame(pos, evaluator.wd)
  }

  /**
    * An exception containing a message, which is expected to get caught by
    * the nearest enclosing try-catch and converted into an [[Error]]
    */
  case class Delegate(msg: String) extends Exception(msg)

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
