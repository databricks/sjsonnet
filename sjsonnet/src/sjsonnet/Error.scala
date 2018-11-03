package sjsonnet

import fastparse.IndexedParserInput

case class DelegateError(msg: String) extends Exception(msg)
case class Error(msg: String,
                 stack: List[StackTraceElement],
                 underlying: Option[Throwable])
  extends Exception(msg, underlying.orNull){
  setStackTrace(stack.toArray.reverse)
  def addFrame(fileName: os.Path, wd: os.Path, offset: Int) = {
    val newFrame = if (os.exists(fileName)){
      val Array(line, col) =
        new IndexedParserInput(os.read(fileName)).prettyIndex(offset).split(':')

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