package sjsonnet

object Platform{
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala.js")
  }
  def patternMatches(pattern: String, str: String): Boolean = {
    val re2 = new RE2(pattern)
    re2.`match`(str)
  }
  def patternFind(pattern: String, str: String): Boolean = {
    throw new Exception("RE2 Regular Expressions are not implemented in Scala.js")
  }
  def patternQuote(str: String): String = {
    throw new Exception("RE2 Regular Expressions are not implemented in Scala.js")
  }
  def patternReplaceFirst(pattern: String, str: String, to: String): String = {
    throw new Exception("RE2 Regular Expressions are not implemented in Scala.js")
  }
  def patternReplaceAll(pattern: String, str: String, to: String): String = {
    throw new Exception("RE2 Regular Expressions are not implemented in Scala.js")
  }
}