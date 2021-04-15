package sjsonnet

import java.util.BitSet

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
  def addFrame(fileName: Path, wd: Path, offset: Int)(implicit ev: EvalErrorScope) = {
    val newFrame = ev.importer.read(fileName) match{
      case None =>
        new StackTraceElement(
          "", "",
          fileName.relativeToString(wd) + " offset:",
          offset
        )
      case Some(resolved) =>
        val Array(line, col) =
          new IndexedParserInput(resolved).prettyIndex(offset).split(':')

        new StackTraceElement(
          "", "",
          fileName.relativeToString(wd) + ":" + line,
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
      throw new Error(e.msg, Nil, None)
        .addFrame(pos.currentFile, evaluator.wd, pos.offset)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e))
        .addFrame(pos.currentFile, evaluator.wd, pos.offset)
  }
  def tryCatchWrap[T](pos: Position)
                     (implicit evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(pos.currentFile, evaluator.wd, pos.offset)
    case e: Error.Delegate =>
      throw new Error(e.msg, Nil, None)
        .addFrame(pos.currentFile, evaluator.wd, pos.offset)
    case NonFatal(e) =>
      throw new Error("Internal Error", Nil, Some(e))
        .addFrame(pos.currentFile, evaluator.wd, pos.offset)
  }
  def fail(msg: String, pos: Position)
          (implicit evaluator: EvalErrorScope) = {
    throw Error(msg, Nil, None).addFrame(pos.currentFile, evaluator.wd, pos.offset)
  }

  def failIfNonEmpty(names: BitSet,
                     outerPos: Position,
                     formatMsg: (String, String) => String,
                     // Allows the use of a custom file scope for computing the error message
                     // for details see: https://github.com/databricks/sjsonnet/issues/83
                     fileScope: FileScope)
                    (implicit eval: EvalErrorScope) = if (!names.isEmpty) {
    val plural = if (names.cardinality() > 1) "s" else ""
    val nameSnippet = BitSetUtils.iterator(names).map(i => fileScope.indexNames(i)).mkString(", ")
    fail(formatMsg(plural, nameSnippet), outerPos)
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
}
