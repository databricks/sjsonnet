package sjsonnet


object Util {
  def tryCatch[T](offset: Int)
                 (implicit fileScope: FileScope, evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e
    case e: DelegateError =>
      throw new Error(e.msg, Nil, None)
        .addFrame(fileScope.currentFile, evaluator.wd, offset)
    case e: Throwable =>
      throw new Error("Internal Error", Nil, Some(e))
        .addFrame(fileScope.currentFile, evaluator.wd, offset)
  }
  def tryCatch2[T](offset: Int)
                  (implicit fileScope: FileScope, evaluator: EvalErrorScope): PartialFunction[Throwable, Nothing] = {
    case e: Error => throw e.addFrame(fileScope.currentFile, evaluator.wd, offset)
    case e: DelegateError =>
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

  def failIfNonEmpty(names: Iterable[String],
                     outerOffset: Int,
                     formatMsg: (String, String) => String)
                    (implicit fileScope: FileScope, eval: EvalErrorScope) = if (names.nonEmpty){
    val plural = if (names.size > 1) "s" else ""
    val nameSnippet = names.mkString(", ")
    Util.fail(formatMsg(plural, nameSnippet), outerOffset)
  }
}

class FileScope(val currentFile: Path,
                val currentRoot: Path,
                val nameIndices: Map[String, Int]){
  val indexNames = nameIndices.map(_.swap)
}

class EvalErrorScope(val extVars: Map[String, ujson.Value], val wd: Path)