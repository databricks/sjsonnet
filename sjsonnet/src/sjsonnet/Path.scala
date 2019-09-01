package sjsonnet

trait Path {
  def relativeToString(p: Path): String
  def debugRead(): Option[String]
  def parent(): Path
  def segmentCount(): Int
  def last: String
  def /(s: String): Path
}
