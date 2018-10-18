package sjsonnet

import ammonite.ops.Path

import fastparse.IndexedParserInput

case class DelegateError(msg: String) extends Exception(msg)
case class Error(msg: String,
                 stack: List[StackTraceElement],
                 underlying: Option[Throwable])
  extends Exception(msg, underlying.orNull){
  setStackTrace(stack.toArray.reverse)
  def addFrame(fileName: Path, wd: Path, offset: Int) = {
    val newFrame = if (ammonite.ops.exists(fileName)){
      val Array(line, col) =
        new IndexedParserInput(ammonite.ops.read(fileName)).prettyIndex(offset).split(':')

      new StackTraceElement(
        "", "",
        fileName.relativeTo(wd).toString + ":" + line,
        col.toInt
      )
    }else{
      new StackTraceElement(
        "", "",
        fileName.relativeTo(wd).toString + " offset:",
        offset
      )
    }


    this.copy(stack = newFrame :: this.stack)
  }
}