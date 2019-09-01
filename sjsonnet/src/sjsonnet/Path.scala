package sjsonnet

trait Path {
  def relativeTo(p: Path): String
  def read(): Option[String]
  def parent(): Path
  def segmentCount(): Int
  def last: String
  def /(s: String): Path
}
