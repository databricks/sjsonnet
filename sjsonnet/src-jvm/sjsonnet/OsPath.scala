package sjsonnet

case class OsPath(p: os.Path) extends Path{
  def relativeTo(other: Path): String = p.relativeTo(other.asInstanceOf[OsPath].p).toString
  def parent(): OsPath = OsPath(p / os.up)
  def segmentCount() = p.segmentCount
  def read(): Option[String] = try Some(os.read(p)) catch{case e => None}
  def last: String = p.last
  def /(s: String): Path = OsPath(p / s)
}
