package sjsonnet

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("re2", "RE2")
@js.native
class RE2(val pattern: String) extends js.Object {
  def `match`(str: String): Boolean = js.native
}
