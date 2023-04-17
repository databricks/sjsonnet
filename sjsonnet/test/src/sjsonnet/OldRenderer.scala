package sjsonnet
import java.io.Writer

import upickle.core.{ArrVisitor, ObjVisitor}

/**
  * Custom JSON renderer to try and match the behavior of google/jsonnet's
  * render:
  *
  * - Custom printing of Doubles
  * - Custom printing of empty dictionaries and arrays
  *
  */
class OldRenderer(out: Writer = new java.io.StringWriter(),
                  indent: Int = -1) extends BaseCharRenderer(out, indent){
  var newlineBuffered = false
  override def visitFloat64(d: Double, index: Int) = {
    flushBuffer()
    out.append(RenderUtils.renderDouble(d))
    out
  }
  val colonSnippet = ": "
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
  override def visitArray(length: Int, index: Int) = new ArrVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    out.append('[')
    newlineBuffered = true

    depth += 1
    def subVisitor = OldRenderer.this
    def visitValue(v: Writer, index: Int): Unit = {
      empty = false
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      newlineBuffered = false
      depth -= 1

      if (empty) out.append(' ')
      else renderIndent()
      out.append(']')
      out
    }
  }

  override def visitJsonableObject(length: Int, index: Int) = new ObjVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    out.append('{')
    newlineBuffered = true
    depth += 1
    def subVisitor = OldRenderer.this
    def visitKey(index: Int) = OldRenderer.this
    def visitKeyValue(v: Any): Unit = {
      empty = false
      flushBuffer()
      out.append(colonSnippet)
    }
    def visitValue(v: Writer, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      newlineBuffered = false
      depth -= 1

      if (empty) out.append(' ')
      else renderIndent()
      out.append('}')
      out
    }
  }
}


class OldPythonRenderer(out: Writer = new java.io.StringWriter(),
                        indent: Int = -1) extends BaseCharRenderer(out, indent){

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

  val colonSnippet = ": "
  override def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(", ")
      renderIndent()
    }
  }
}
