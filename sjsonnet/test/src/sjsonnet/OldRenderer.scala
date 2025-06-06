package sjsonnet
import java.io.Writer

import upickle.core.{ArrVisitor, ObjVisitor}

/**
 * Custom JSON renderer to try and match the behavior of google/jsonnet's render:
 *
 *   - Custom printing of Doubles
 *   - Custom printing of empty dictionaries and arrays
 */
class OldRenderer(out: Writer = new java.io.StringWriter(), indent: Int = -1)
    extends BaseRenderer(out, indent) {
  var newlineBuffered = false
  override def visitFloat64(d: Double, index: Int): Writer = {
    flushBuffer()
    out.append(RenderUtils.renderDouble(d))
    out
  }
  override val colonSnippet = ": "
  override def flushBuffer(): Unit = {
    if (commaBuffered) {
      if (indent == -1) out.append(", ")
      else out.append(',')
    }
    if (indent == -1) ()
    else if (commaBuffered || newlineBuffered) {
      out.append('\n')

      var i = indent * depth
      while (i > 0) {
        out.append(' ')
        i -= 1
      }
    }
    newlineBuffered = false
    commaBuffered = false
  }
  override def visitArray(
      length: Int,
      index: Int): upickle.core.ArrVisitor[java.io.Writer, java.io.Writer] {
    def subVisitor: sjsonnet.OldRenderer
  } = new ArrVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    out.append('[')
    newlineBuffered = true

    depth += 1
    def subVisitor: sjsonnet.OldRenderer = OldRenderer.this
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

  override def visitObject(
      length: Int,
      index: Int): upickle.core.ObjVisitor[java.io.Writer, java.io.Writer] {
    def subVisitor: sjsonnet.OldRenderer; def visitKey(index: Int): sjsonnet.OldRenderer
  } = new ObjVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    out.append('{')
    newlineBuffered = true
    depth += 1
    def subVisitor: sjsonnet.OldRenderer = OldRenderer.this
    def visitKey(index: Int): sjsonnet.OldRenderer = OldRenderer.this
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

class OldPythonRenderer(out: Writer = new java.io.StringWriter(), indent: Int = -1)
    extends BaseRenderer(out, indent) {

  override def visitNull(index: Int): Writer = {
    flushBuffer()
    out.append("None")
    out
  }

  override def visitFalse(index: Int): Writer = {
    flushBuffer()
    out.append("False")
    out
  }

  override def visitTrue(index: Int): Writer = {
    flushBuffer()
    out.append("True")
    out
  }

  override val colonSnippet = ": "
  override def flushBuffer(): Unit = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(", ")
      renderIndent()
    }
  }
}
