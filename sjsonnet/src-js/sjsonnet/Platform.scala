package sjsonnet

import scala.scalajs.js
import scala.scalajs.js.RegExp
import scala.scalajs.js.annotation.JSImport

package re2 {

  @js.native
  @JSImport("re2", JSImport.Namespace)
  class RE2(pattern: String, flags: String) extends RegExp(pattern, flags) {
    def `match`(str: String): AnyRef = js.native
    def replace(str: String, to: String): AnyRef = js.native
  }
}

object Platform{
  def md5(s: String): String = {
    throw new Exception("MD5 not implemented in Scala.js")
  }

  def patternMatches(pattern: String, str: String): Boolean = {
    val matchStr = new re2.RE2(pattern, "g").`match`(str)
    matchStr != null && matchStr.toString == str
  }
  def patternFind(pattern: String, str: String): Boolean = {
    val matchStr = new re2.RE2(pattern, "").`match`(str)
    matchStr != null
  }
  //This functionality does not exist in node-re2 so copy&paste from RE2
  def patternQuote(str: String): String = {
    val b = new StringBuilder(2 * str.length)
    // A char loop is correct because all metacharacters fit in one UTF-16 code.
    var i = 0
    val len = str.length
    while ( {
      i < len
    }) {
      val c = str.charAt(i)
      if ("\\.+*?()|[]{}^$".indexOf(c) >= 0) b.append('\\')
      b.append(c)

      {
        i += 1; i - 1
      }
    }
    return b.toString
  }
  def patternReplaceFirst(pattern: String, str: String, to: String): String = {
    new re2.RE2(pattern, "").replace(str, to).toString
  }

  def patternReplaceAll(pattern: String, str: String, to: String): String = {
    new re2.RE2(pattern, "g").replace(str, to).toString
  }
}