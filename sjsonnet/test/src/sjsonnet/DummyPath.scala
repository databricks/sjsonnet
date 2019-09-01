package sjsonnet

case class DummyPath(segments: String*) extends Path{
  def relativeTo(p: Path): String = ""

  def read(): Option[String] = None

  def parent(): Path = DummyPath(segments.dropRight(1):_*)

  def segmentCount(): Int = segments.length

  def last: String = segments.last

  def /(s: String): Path = DummyPath(segments :+ s:_*)
}
