package sjsonnet
object Platform{
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala.js")
  }
}