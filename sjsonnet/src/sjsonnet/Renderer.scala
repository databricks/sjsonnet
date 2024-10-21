package sjsonnet

import java.io.{StringWriter, Writer}

import upickle.core.{ArrVisitor, ObjVisitor}

/**
  * Custom JSON renderer to try and match the behavior of google/jsonnet's
  * render:
  *
  * - Custom printing of Doubles
  * - Custom printing of empty dictionaries and arrays
  *
  */
class Renderer(out: Writer = new java.io.StringWriter(),
               indent: Int = -1) extends BaseCharRenderer(out, indent){
  var newlineBuffered = false
  override def visitFloat64(d: Double, index: Int) = {
    val s = RenderUtils.renderDouble(d)
    flushBuffer()
    var i = 0
    val sLength = s.length
    elemBuilder.ensureLength(sLength)
    while(i < sLength){
      elemBuilder.appendUnsafeC(s.charAt(i))
      i += 1
    }
    flushCharBuilder()
    out
  }
  override def flushBuffer() = {
    if (commaBuffered) {
      elemBuilder.append(',')
      if (indent == -1) elemBuilder.append(' ')
    }
    if (indent == -1) ()
    else if (commaBuffered || newlineBuffered) {
      var i = indent * depth
      elemBuilder.ensureLength(i+1)
      elemBuilder.append('\n')
      while(i > 0) {
        elemBuilder.append(' ')
        i -= 1
      }
    }
    newlineBuffered = false
    commaBuffered = false
  }
  override def visitArray(length: Int, index: Int) = new ArrVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    elemBuilder.append('[')
    newlineBuffered = true

    depth += 1
    def subVisitor = Renderer.this
    def visitValue(v: Writer, index: Int): Unit = {
      empty = false
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      newlineBuffered = false
      depth -= 1

      if (empty) elemBuilder.append(' ')
      else renderIndent()
      elemBuilder.append(']')
      flushCharBuilder()
      out
    }
  }

  override def visitJsonableObject(length: Int, index: Int) = new ObjVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    elemBuilder.append('{')
    newlineBuffered = true
    depth += 1
    def subVisitor = Renderer.this
    def visitKey(index: Int) = Renderer.this
    def visitKeyValue(v: Any): Unit = {
      empty = false
      //flushBuffer()
      elemBuilder.append(':')
      elemBuilder.append(' ')
    }
    def visitValue(v: Writer, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      newlineBuffered = false
      depth -= 1

      if (empty) elemBuilder.append(' ')
      else renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      out
    }
  }
}


class PythonRenderer(out: Writer = new java.io.StringWriter(),
                     indent: Int = -1) extends BaseCharRenderer(out, indent){

  override def visitNull(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('N')
    elemBuilder.appendUnsafe('o')
    elemBuilder.appendUnsafe('n')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  override def visitFalse(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(5)
    elemBuilder.appendUnsafe('F')
    elemBuilder.appendUnsafe('a')
    elemBuilder.appendUnsafe('l')
    elemBuilder.appendUnsafe('s')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  override def visitTrue(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('T')
    elemBuilder.appendUnsafe('r')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  override def visitJsonableObject(length: Int, index: Int) = new ObjVisitor[Writer, Writer] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    renderIndent()
    def subVisitor = PythonRenderer.this
    def visitKey(index: Int) = PythonRenderer.this
    def visitKeyValue(s: Any): Unit = {
      elemBuilder.ensureLength(2)
      elemBuilder.append(':')
      elemBuilder.append(' ')
    }
    def visitValue(v: Writer, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      out
    }
  }

  override def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      elemBuilder.ensureLength(2)
      elemBuilder.append(',')
      elemBuilder.append(' ')
      renderIndent()
    }
  }
}

/** Renderer used by std.manifestJson and std.manifestJsonEx */
case class MaterializeJsonRenderer(indent: Int = 4, escapeUnicode: Boolean = false, out: StringWriter = new StringWriter())
  extends BaseCharRenderer(out, indent, escapeUnicode) {

  override def visitArray(length: Int, index: Int) = new ArrVisitor[StringWriter, StringWriter] {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if(length == 0) elemBuilder.append('\n') else renderIndent()
    def subVisitor = MaterializeJsonRenderer.this
    def visitValue(v: StringWriter, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append(']')
      flushCharBuilder()
      out
    }
  }

  override def visitJsonableObject(length: Int, index: Int) = new ObjVisitor[StringWriter, StringWriter] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if(length == 0) elemBuilder.append('\n') else renderIndent()
    def subVisitor = MaterializeJsonRenderer.this
    def visitKey(index: Int) = MaterializeJsonRenderer.this
    def visitKeyValue(s: Any): Unit = {
      elemBuilder.append(':')
      if (indent != -1) elemBuilder.append(' ')
    }
    def visitValue(v: StringWriter, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append('}')
      flushCharBuilder()
      out
    }
  }
}

object RenderUtils {
  /**
    * Custom rendering of Doubles used in rendering
    */
  def renderDouble(d: Double): String = {
    if (d.toLong == d) d.toLong.toString
    else if (d % 1 == 0) {
      BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt.toString()
    }
    else d.toString
  }
}
