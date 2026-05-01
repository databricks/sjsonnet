package sjsonnet

import scala.collection.mutable

/**
 * Walks an [[Expr]] AST collecting all `import`, `importstr`, and `importbin` expressions. Used by
 * [[Preloader]] to discover the transitive set of files that need to be loaded before evaluation.
 */
object ImportFinder {

  sealed trait Kind {

    /**
     * Whether the file should be read as raw bytes (`importbin`) vs. text (`import`/`importstr`).
     */
    def binaryData: Boolean

    /** Whether the loaded file is itself Jsonnet code that may contain further imports. */
    def isCode: Boolean
  }

  object Kind {
    case object Code extends Kind {
      def binaryData: Boolean = false
      def isCode: Boolean = true
    }
    case object Str extends Kind {
      def binaryData: Boolean = false
      def isCode: Boolean = false
    }
    case object Bin extends Kind {
      def binaryData: Boolean = true
      def isCode: Boolean = false
    }
  }

  final case class Found(value: String, kind: Kind)

  def collect(expr: Expr): Seq[Found] = {
    val buf = mutable.ArrayBuffer.empty[Found]
    val walker = new Walker(buf)
    walker.transform(expr)
    buf.toSeq
  }

  private class Walker(buf: mutable.ArrayBuffer[Found]) extends ExprTransform {
    override def transform(expr: Expr): Expr = {
      expr match {
        case Expr.Import(_, v)    => buf += Found(v, Kind.Code)
        case Expr.ImportStr(_, v) => buf += Found(v, Kind.Str)
        case Expr.ImportBin(_, v) => buf += Found(v, Kind.Bin)
        case _                    =>
      }
      rec(expr)
    }
  }
}
