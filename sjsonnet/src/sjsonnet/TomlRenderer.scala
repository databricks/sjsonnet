package sjsonnet

import upickle.core.{ArrVisitor, CharBuilder, ObjVisitor, SimpleVisitor, Visitor}

import java.io.StringWriter


class TomlRenderer(out: StringWriter = new java.io.StringWriter(), cumulatedIndent: String, indent: String) extends SimpleVisitor[StringWriter, StringWriter]{
  override def expectedMsg: String = "unimplemented type in Materializer"
  private object objectKeyRenderer extends upickle.core.SimpleVisitor[StringWriter, StringWriter] {
    override def expectedMsg = "expected string"

    override def visitNull(index: Int): StringWriter = {
      TomlRenderer.this.visitNull(index)
    }

    override def visitString(s: CharSequence, index: Int): StringWriter = {
      if (s == null) visitNull(index)
      else {
        out.write(TomlRenderer.escapeKey(s.toString))
        out
      }
    }
  }

  private var depth = 0

  private def flush = {
    if (depth == 0) out.write("\n")
    out.flush()
    out
  }

  override def visitNull(index: Int): StringWriter = Error.fail("Tried to manifest \"null\"")

  override def visitTrue(index: Int): StringWriter = {
    out.write("true")
    flush
  }

  override def visitFalse(index: Int): StringWriter = {
    out.write("false")
    flush
  }

  override def visitString(s: CharSequence, index: Int): StringWriter = {
    if (s == null) {
      visitNull(index)
    } else {
      val charBuilder = new CharBuilder()
      upickle.core.RenderUtils.escapeChar(null, charBuilder, s, unicode = true)
      out.write(charBuilder.makeString())
      flush
    }
  }

  override def visitFloat64(d: Double, index: Int): StringWriter = {
    d match {
      case Double.PositiveInfinity => out.write("inf")
      case Double.NegativeInfinity => out.write("-inf")
      case d if java.lang.Double.isNaN(d) => out.write("nan")
      case d if math.round(d).toDouble == d => out.write(java.lang.Long.toString(d.toLong))
      case d => out.write(java.lang.Double.toString(d))
    }
    flush
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[StringWriter, StringWriter] = new ArrVisitor[StringWriter, StringWriter] {
    private val inline = length == 0 || depth > 0
    private val newElementIndent = if (inline) "" else cumulatedIndent + indent
    private val separator = if (inline && length > 0) " " else if (inline && length == 0) "" else "\n"
    private var addComma = false

    depth += 1
    out.write("[" + separator)
    def subVisitor: Visitor[StringWriter, StringWriter] = {
      if (addComma) out.write("," + separator)
      out.write(newElementIndent)
      TomlRenderer.this
    }
    def visitValue(v: StringWriter, index: Int): Unit = {
      addComma = true
    }
    def visitEnd(index: Int): StringWriter = {
      addComma = false
      depth -= 1
      out.write(separator)
      if (!inline) out.write(cumulatedIndent)
      out.write("]")
      flush
    }
  }

  override def visitObject(length: Int, index: Int): ObjVisitor[StringWriter, StringWriter] = new ObjVisitor[StringWriter, StringWriter] {
    private var addComma = false
    depth += 1
    out.write("{ ")
    def subVisitor: Visitor[StringWriter, StringWriter] = TomlRenderer.this
    def visitKey(index: Int): Visitor[StringWriter, StringWriter] = {
      if (addComma) out.write(", ")
      objectKeyRenderer
    }
    def visitKeyValue(s: Any): Unit = {
      out.write(" = ")
    }
    def visitValue(v: StringWriter, index: Int): Unit = {
      addComma = true
    }
    def visitEnd(index: Int): StringWriter = {
      addComma = false
      depth -= 1
      out.write(" }")
      flush
    }
  }
}

object TomlRenderer {
  private val bareAllowed = Platform.getPatternFromCache("[A-Za-z0-9_-]+")
  def escapeKey(key: String): String = if (bareAllowed.matcher(key).matches()) key else {
    val out = new StringWriter()
    BaseRenderer.escape(out, key, unicode = true)
    out.toString
  }
}
