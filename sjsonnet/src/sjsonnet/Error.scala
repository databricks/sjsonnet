package sjsonnet

import fastparse.IndexedParserInput

case class DelegateError(msg: String) extends Exception(msg)
case class Error(msg: String,
                 stack: List[StackTraceElement],
                 underlying: Option[Throwable])
  extends Exception(msg, underlying.orNull){
  setStackTrace(stack.toArray.reverse)
  def addFrame(fileName: Path, wd: Path, offset: Int) = {
    val newFrame = fileName.read() match{
      case None =>
        new StackTraceElement(
          "", "",
          fileName.relativeTo(wd).toString + " offset:",
          offset
        )
      case Some(resolved) =>
        val Array(line, col) =
          new IndexedParserInput(resolved).prettyIndex(offset).split(':')

        new StackTraceElement(
          "", "",
          fileName.relativeTo(wd).toString + ":" + line,
          col.toInt
        )
    }

    this.copy(stack = newFrame :: this.stack)
  }
}