package sjsonnet

import ammonite.ops.Path
import fastparse.StringReprOps
import fastparse.utils.IndexedParserInput

case class DelegateError(msg: String) extends Exception(msg)
case class Error(msg: String,
                 stack: List[StackTraceElement],
                 underlying: Option[Throwable])
  extends Exception(msg, underlying.orNull){
  setStackTrace(stack.toArray.reverse)
  def addFrame(fileName: Path, offset: Int) = {
    val Array(line, col) = StringReprOps.prettyIndex(
      new IndexedParserInput(ammonite.ops.read(fileName))(StringReprOps), offset
    ).split(':')

    val newFrame = new StackTraceElement(
      "", "",
      fileName.relativeTo(ammonite.ops.pwd).toString + ":" + line,
      col.toInt
    )

    this.copy(stack = newFrame :: this.stack)
  }
}