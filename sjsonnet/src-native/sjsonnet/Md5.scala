package sjsonnet

import scala.scalanative.unsafe._
import scala.scalanative.libc.string.strlen
import scala.scalanative.runtime.ByteArray

object Md5 {
  // @link("crypto") // Linking using nativeLinkingOptions in build.sc instead
  @extern
  private object C {
    def MD5(string: CString, size: CSize, result: CString): CString = extern
  }

  def md5(s: String): Array[Byte] = {
    val result = ByteArray.alloc(16)
    Zone { implicit z =>
      val cstring = toCString(s)
      C.MD5(cstring, strlen(cstring), result.at(0))
    }
    result.asInstanceOf[Array[Byte]]
  }
}
