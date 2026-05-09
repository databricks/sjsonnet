package sjsonnet

import scala.collection.mutable

/** The kind of import expression that referenced a file. */
sealed trait ImportKind {

  /** Whether the file should be read as raw bytes (`importbin`) vs. text (`import`/`importstr`). */
  def binaryData: Boolean

  /** Whether the loaded file is itself Jsonnet code that may contain further imports. */
  def isCode: Boolean
}

object ImportKind {
  case object Code extends ImportKind {
    def binaryData: Boolean = false
    def isCode: Boolean = true
  }
  case object Str extends ImportKind {
    def binaryData: Boolean = false
    def isCode: Boolean = false
  }
  case object Bin extends ImportKind {
    def binaryData: Boolean = true
    def isCode: Boolean = false
  }
}

/**
 * Walks an [[Expr]] AST collecting all `import`, `importstr`, and `importbin` expressions. Used by
 * [[Preloader]] to discover the transitive set of files that need to be loaded before evaluation.
 */
object ImportFinder {

  final case class Found(value: String, kind: ImportKind)

  def collect(expr: Expr): Seq[Found] = {
    val buf = mutable.ArrayBuffer.empty[Found]
    val walker = new Walker(buf)
    walker.transform(expr)
    buf.toSeq
  }

  private class Walker(buf: mutable.ArrayBuffer[Found]) extends ExprTransform {
    override def transform(expr: Expr): Expr = {
      expr match {
        case Expr.Import(_, v)    => buf += Found(v, ImportKind.Code)
        case Expr.ImportStr(_, v) => buf += Found(v, ImportKind.Str)
        case Expr.ImportBin(_, v) => buf += Found(v, ImportKind.Bin)
        case _                    =>
      }
      rec(expr)
    }
  }
}
