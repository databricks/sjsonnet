package sjsonnet
import java.io.StringWriter

import ujson.BaseRenderer

class Renderer(out: StringWriter = new java.io.StringWriter(),
               indent: Int = -1) extends BaseRenderer(out, indent){
  override def visitNumRaw(d: Double, index: Int) = {
    out.append(new java.math.BigDecimal(d).toPlainString)
    flushBuffer()
    out
  }
}
