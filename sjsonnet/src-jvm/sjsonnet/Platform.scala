package sjsonnet

import com.google.re2j.Pattern

object Platform {
  def md5(s: String): String = {
    java.security.MessageDigest.getInstance("MD5")
      .digest(s.getBytes("UTF-8"))
      .map{ b => String.format("%02x", new java.lang.Integer(b & 0xff))}
      .mkString
  }

  def patternMatches(pattern: String, str: String): Boolean = {
    Pattern.matches(pattern, str)
  }
  def patternFind(pattern: String, str: String): Boolean = {
    Pattern.compile(pattern).matcher(str).find()
  }
  def patternQuote(str: String): String = {
    Pattern.quote(str)
  }
  def patternReplaceFirst(pattern: String, str: String, to: String): String = {
    Pattern.compile(pattern).matcher(str).replaceFirst(to)
  }
  def patternReplaceAll(pattern: String, str: String, to: String): String = {
    Pattern.compile(pattern).matcher(str).replaceAll(to)
  }
}