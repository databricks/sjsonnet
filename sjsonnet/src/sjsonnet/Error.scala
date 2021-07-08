package sjsonnet

import fastparse.IndexedParserInput

import scala.util.control.NonFatal

/**
  * An exception that can keep track of the Sjsonnet call-stack while it is
  * propagating upwards. This helps provide good error messages with line
  * numbers pointing towards user code.
  */
class Error(val msg: String,
            val stack: List[StackTraceElement] = Nil,
            val underlying: Option[Throwable] = None)
  extends Exception(msg, underlying.orNull){
  setStackTrace(stack.toArray.reverse)

  override def fillInStackTrace: Throwable = this

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

    new Error(msg, newFrame :: stack, underlying)
  }
}


object Error {
  def tryCatch[T](pos: Position)
                 (implicit evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error if e.stack.isEmpty => throw e.addFrame(pos, evaluator.wd)
    case e: Error => throw e
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e)).addFrame(pos, evaluator.wd)
  }
  def tryCatchWrap[T](pos: Position)
                     (implicit evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(pos, evaluator.wd)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e)).addFrame(pos, evaluator.wd)
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
