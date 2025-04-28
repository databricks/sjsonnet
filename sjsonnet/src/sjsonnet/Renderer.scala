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
  override def visitFloat64(d: Double, index: Int): Writer = {
    val s = RenderUtils.renderDouble(d)
    flushBuffer()
    appendString(s)
    flushCharBuilder()
    out
  }
  override def flushBuffer(): Unit = {
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
  override def visitArray(length: Int, index: Int): upickle.core.ArrVisitor[java.io.Writer,java.io.Writer]{def subVisitor: sjsonnet.Renderer} = new ArrVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    elemBuilder.append('[')
    newlineBuffered = true

    depth += 1
    def subVisitor: sjsonnet.Renderer = Renderer.this
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

  override def visitObject(length: Int, index: Int): upickle.core.ObjVisitor[java.io.Writer,java.io.Writer]{def subVisitor: sjsonnet.Renderer; def visitKey(index: Int): sjsonnet.Renderer} = new ObjVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    elemBuilder.append('{')
    newlineBuffered = true
    depth += 1
    def subVisitor: sjsonnet.Renderer= Renderer.this
    def visitKey(index: Int): sjsonnet.Renderer = Renderer.this
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

  override def visitNull(index: Int): Writer = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('N')
    elemBuilder.appendUnsafe('o')
    elemBuilder.appendUnsafe('n')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  override def visitFalse(index: Int): Writer = {
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

  override def visitTrue(index: Int): Writer = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('T')
    elemBuilder.appendUnsafe('r')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  override def visitObject(length: Int, index: Int): upickle.core.ObjVisitor[java.io.Writer,java.io.Writer]{def subVisitor: sjsonnet.PythonRenderer; def visitKey(index: Int): sjsonnet.PythonRenderer} = new ObjVisitor[Writer, Writer] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    renderIndent()
    def subVisitor: sjsonnet.PythonRenderer = PythonRenderer.this
    def visitKey(index: Int): sjsonnet.PythonRenderer = PythonRenderer.this
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

  override def flushBuffer(): Unit = {
    if (commaBuffered) {
      commaBuffered = false
      elemBuilder.ensureLength(2)
      elemBuilder.append(',')
      elemBuilder.append(' ')
      renderIndent()
    }
  }
}

/** Renderer used by std.manifestJson, std.manifestJsonMinified, and std.manifestJsonEx */
final case class MaterializeJsonRenderer(indent: Int = 4, escapeUnicode: Boolean = false, out: StringWriter = new StringWriter(), newline: String = "\n", keyValueSeparator: String = ": ")
  extends BaseCharRenderer(out, indent, escapeUnicode, newline.toCharArray) {
  private val newLineCharArray = newline.toCharArray
  private val keyValueSeparatorCharArray = keyValueSeparator.toCharArray

  override def visitArray(length: Int, index: Int): upickle.core.ArrVisitor[java.io.StringWriter,java.io.StringWriter]{def subVisitor: sjsonnet.MaterializeJsonRenderer} = new ArrVisitor[StringWriter, StringWriter] {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if (length == 0 && indent != -1) elemBuilder.appendAll(newLineCharArray, newLineCharArray.length) else renderIndent()
    def subVisitor: sjsonnet.MaterializeJsonRenderer = MaterializeJsonRenderer.this
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

  override def visitObject(length: Int, index: Int): upickle.core.ObjVisitor[java.io.StringWriter,java.io.StringWriter]{def subVisitor: sjsonnet.MaterializeJsonRenderer; def visitKey(index: Int): sjsonnet.MaterializeJsonRenderer} = new ObjVisitor[StringWriter, StringWriter] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    // account for rendering differences of whitespaces in ujson and jsonnet manifestJson
    if (length == 0 && indent != -1) elemBuilder.appendAll(newLineCharArray, newLineCharArray.length) else renderIndent()
    def subVisitor: sjsonnet.MaterializeJsonRenderer = MaterializeJsonRenderer.this
    def visitKey(index: Int): sjsonnet.MaterializeJsonRenderer = MaterializeJsonRenderer.this
    def visitKeyValue(s: Any): Unit = {
      elemBuilder.appendAll(keyValueSeparatorCharArray, keyValueSeparatorCharArray.length)
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
