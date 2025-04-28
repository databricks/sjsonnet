package sjsonnet

import scala.collection.mutable

final case class OsPath(p: os.Path) extends Path{
  def relativeToString(other: Path): String = p.relativeTo(other.asInstanceOf[OsPath].p).toString
  def parent(): OsPath = OsPath(p / os.up)
  def segmentCount() = p.segmentCount
  def debugRead(): Option[String] = try Some(os.read(p)) catch{case e: Throwable => None}
  def last: String = p.last
  def /(s: String): Path = OsPath(p / s)

  override def equals(other: Any): Boolean = other match {
    case OsPath(p2) => p == p2
    case _ => false
  }

  override def hashCode: Int = p.hashCode()

  def renderOffsetStr(offset: Int, loadedFileContents: mutable.HashMap[Path, Array[Int]]): String = {
    val offsetStr =
      if (p.toString.contains("(materialize)")) ""
      else {
        val lineStarts = loadedFileContents.getOrElseUpdate(
          this,
          fastparse.internal.Util.lineNumberLookup(os.read(p))
        )

        ":" + Util.prettyIndex(lineStarts, offset)
      }

    p.relativeTo(os.pwd).toString + offsetStr
  }
}