package sjsonnet

import fastparse.IndexedParserInput

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
    val newFrame = ev.loadCachedSource(fileName) match{
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
  def tryCatch[T](offset: Int)
                 (implicit fileScope: FileScope, evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e
    case e: Error.Delegate =>
      throw new Error(e.msg, Nil, None)
        .addFrame(fileScope.currentFile, evaluator.wd, offset)
    case e: Throwable =>
      throw new Error("Internal Error", Nil, Some(e))
        .addFrame(fileScope.currentFile, evaluator.wd, offset)
  }
  def tryCatchWrap[T](offset: Int)
                     (implicit fileScope: FileScope, evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(fileScope.currentFile, evaluator.wd, offset)
    case e: Error.Delegate =>
      throw new Error(e.msg, Nil, None)
        .addFrame(fileScope.currentFile, evaluator.wd, offset)
    case e: Throwable =>
      throw new Error("Internal Error", Nil, Some(e))
        .addFrame(fileScope.currentFile, evaluator.wd, offset)
  }
  def fail(msg: String, offset: Int)
          (implicit fileScope: FileScope, evaluator: EvalErrorScope) = {
    throw Error(msg, Nil, None).addFrame(fileScope.currentFile, evaluator.wd, offset)
  }

  def failIfNonEmpty(names: collection.BitSet,
                     outerOffset: Int,
                     formatMsg: (String, String) => String)
                    (implicit fileScope: FileScope, eval: EvalErrorScope) = if (names.nonEmpty){
    val plural = if (names.size > 1) "s" else ""
    val nameSnippet = names.map(fileScope.indexNames).mkString(", ")
    fail(formatMsg(plural, nameSnippet), outerOffset)
  }


  /**
    * An exception containing a message, which is expected to get caught by
    * the nearest enclosing try-catch and converted into an [[Error]]
    */
  case class Delegate(msg: String) extends Exception(msg)

}

/**
  * FileScope models the per-file context that is propagated throughout the
  * evaluation of a single Jsonnet file. Contains the current file path, as
  * well as the mapping of local variable names to local variable array indices
  * which is shared throughout each file.
  */
class FileScope(val currentFile: Path,
                val nameIndices: Map[String, Int]){
  // Only used for error messages, so in the common case
  // where nothing blows up this does not need to be allocated
  lazy val indexNames = nameIndices.map(_.swap)
}

trait EvalErrorScope {
  def extVars: Map[String, ujson.Value]
  def loadCachedSource(p: Path): Option[String]
  def wd: Path
}
