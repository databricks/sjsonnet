package sjsonnet

import fastparse.IndexedParserInput

case class DelegateError(msg: String) extends Exception(msg)
case class Error(msg: String,
                 stack: List[StackTraceElement],
                 underlying: Option[Throwable])
  extends Exception(msg, underlying.orNull){
  setStackTrace(stack.toArray.reverse)
  def addFrame(fileName: Path, wd: Path, offset: Int) = {
    val newFrame = fileName.debugRead() match{
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