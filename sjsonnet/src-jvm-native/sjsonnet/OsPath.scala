package sjsonnet

case class OsPath(p: os.Path) extends Path{
  def relativeToString(other: Path): String = p.relativeTo(other.asInstanceOf[OsPath].p).toString
  def parent(): OsPath = OsPath(p / os.up)
  def segmentCount() = p.segmentCount
  def debugRead(): Option[String] = try Some(os.read(p)) catch{case e: Throwable => None}
  def last: String = p.last
  def /(s: String): Path = OsPath(p / s)
}
