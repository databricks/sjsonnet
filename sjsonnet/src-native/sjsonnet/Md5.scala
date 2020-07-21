package sjsonnet

import scala.scalanative.unsafe._
import scala.scalanative.libc.string.strlen
import scala.scalanative.runtime.ByteArray

object Md5 {
  // @link("crypto") // Linking using nativeLinkingOptions in build.sc instead
  @extern
  private object C {
    def MD5(string: Ptr[Byte], size: CSize, result: Ptr[Byte]): Ptr[Byte] = extern
  }

  def md5(s: String): Array[Byte] = {
    val result = ByteArray.alloc(16)
    val input = s.getBytes("UTF-8")
    C.MD5(input.asInstanceOf[ByteArray].at(0), input.length, result.at(0))
    result.asInstanceOf[Array[Byte]]
  }
}
