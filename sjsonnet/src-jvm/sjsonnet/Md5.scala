package sjsonnet

object Md5 {
  def md5(s: String): Iterable[Byte] = {
    java.security.MessageDigest.getInstance("MD5")
      .digest(s.getBytes("UTF-8"))
  }
}
