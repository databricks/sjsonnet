package sjsonnet
import java.io.StringWriter

import ujson.{ArrVisitor, BaseRenderer, ObjVisitor, Renderer}

class PythonRenderer(out: StringWriter = new java.io.StringWriter(),
                     indent: Int = -1) extends BaseRenderer(out, indent){

  override def visitNull(index: Int) = {
    flushBuffer()
    out.append("None")
    out
  }

  override def visitFalse(index: Int) = {
    flushBuffer()
    out.append("False")
    out
  }

  override def visitTrue(index: Int) = {
    flushBuffer()
    out.append("True")
    out
  }

  override val colonSnippet = ": "
  override def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(", ")
      renderIndent()
    }
  }
}

/**
  * Custom JSON renderer to try and match the behavior of google/jsonnet's
  * render:
  *
  * - Custom printing of Doubles
  * - Custom printing of empty dictionaries and arrays
  *
  */
class Renderer(out: StringWriter = new java.io.StringWriter(),
               indent: Int = -1) extends BaseRenderer(out, indent){
  var newlineBuffered = false
  override def visitNumRaw(d: Double, index: Int) = {
    flushBuffer()
    out.append(new java.math.BigDecimal(d).toPlainString)
    flushBuffer()
    out
  }
  override val colonSnippet = ": "
  override def flushBuffer() = {
    if (commaBuffered) {
      if (indent == -1) out.append(", ")
      else out.append(',')
    }
    if (indent == -1) ()
    else if (commaBuffered || newlineBuffered) {
      out.append('\n')

      var i = indent * depth
      while(i > 0) {
        out.append(' ')
        i -= 1
      }
    }
    newlineBuffered = false
    commaBuffered = false
  }
  override def visitArray(index: Int) = new ArrVisitor[StringWriter, StringWriter] {
    var empty = true
    flushBuffer()
    out.append('[')
    newlineBuffered = true

    depth += 1
    def subVisitor = Renderer.this
    def visitValue(v: StringWriter, index: Int): Unit = {
      empty = false
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1

      if (empty) out.append(' ')
      else renderIndent()
      out.append(']')
      out
    }
  }

  override def visitObject(index: Int) = new ObjVisitor[StringWriter, StringWriter] {
    var empty = true
    flushBuffer()
    out.append('{')
    newlineBuffered = true
    depth += 1
    def subVisitor = Renderer.this
    def visitKey(s: CharSequence, index: Int): Unit = {
      empty = false
      flushBuffer()

      ujson.Renderer.escape(out, s, true)

      out.append(colonSnippet)
    }
    def visitValue(v: StringWriter, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1

      if (empty) out.append(' ')
      else renderIndent()
      out.append('}')
      out
    }
  }
}
