package sjsonnet

import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

// Uses the unsynchronized [[StringBuilderWriter]] rather than java.io.StringWriter: the latter is
// backed by a synchronized StringBuffer, paying a monitor enter/exit on every write/flush on the
// hot manifestTomlEx path. Output is byte-identical. Same swap as the JSON renderer in #874.
class TomlRenderer(
    out: StringBuilderWriter = new StringBuilderWriter(),
    cumulatedIndent: String,
    indent: String)
    extends SimpleVisitor[StringBuilderWriter, StringBuilderWriter] {
  override def expectedMsg: String = "unimplemented type in Materializer"
  private object objectKeyRenderer
      extends upickle.core.SimpleVisitor[StringBuilderWriter, StringBuilderWriter] {
    override def expectedMsg = "expected string"

    override def visitNull(index: Int): StringBuilderWriter = {
      TomlRenderer.this.visitNull(index)
    }

    override def visitString(s: CharSequence, index: Int): StringBuilderWriter = {
      if (s == null) visitNull(index)
      else {
        TomlRenderer.writeEscapedKey(out, s)
        out
      }
    }
  }

  private var depth = 0

  private def flush = {
    if (depth == 0) out.write('\n')
    out
  }

  override def visitNull(index: Int): StringBuilderWriter = Error.fail("Tried to manifest \"null\"")

  override def visitTrue(index: Int): StringBuilderWriter = {
    out.write("true")
    flush
  }

  override def visitFalse(index: Int): StringBuilderWriter = {
    out.write("false")
    flush
  }

  override def visitString(s: CharSequence, index: Int): StringBuilderWriter = {
    if (s == null) {
      visitNull(index)
    } else {
      BaseRenderer.escape(out, s, unicode = true)
      flush
    }
  }

  override def visitFloat64(d: Double, index: Int): StringBuilderWriter = {
    d match {
      case Double.PositiveInfinity          => out.write("inf")
      case Double.NegativeInfinity          => out.write("-inf")
      case d if java.lang.Double.isNaN(d)   => out.write("nan")
      case d if math.round(d).toDouble == d => out.write(java.lang.Long.toString(d.toLong))
      case d                                => out.write(java.lang.Double.toString(d))
    }
    flush
  }

  override def visitArray(
      length: Int,
      index: Int): ArrVisitor[StringBuilderWriter, StringBuilderWriter] =
    new ArrVisitor[StringBuilderWriter, StringBuilderWriter] {
      private val isInLine = length == 0 || depth > 0
      private val newElementIndent = if (isInLine) "" else cumulatedIndent + indent
      private val separator =
        if (isInLine && length > 0) " " else if (isInLine && length == 0) "" else "\n"
      private var addComma = false

      depth += 1
      out.write('[')
      out.write(separator)
      def subVisitor: Visitor[StringBuilderWriter, StringBuilderWriter] = {
        if (addComma) {
          out.write(',')
          out.write(separator)
        }
        out.write(newElementIndent)
        TomlRenderer.this
      }
      def visitValue(v: StringBuilderWriter, index: Int): Unit = {
        addComma = true
      }
      def visitEnd(index: Int): StringBuilderWriter = {
        addComma = false
        depth -= 1
        out.write(separator)
        if (!isInLine) out.write(cumulatedIndent)
        out.write("]")
        flush
      }
    }

  override def visitObject(
      length: Int,
      jsonableKeys: Boolean,
      index: Int): ObjVisitor[StringBuilderWriter, StringBuilderWriter] =
    new ObjVisitor[StringBuilderWriter, StringBuilderWriter] {
      private var addComma = false
      depth += 1
      out.write("{ ")
      def subVisitor: Visitor[StringBuilderWriter, StringBuilderWriter] = TomlRenderer.this
      def visitKey(index: Int): Visitor[StringBuilderWriter, StringBuilderWriter] = {
        if (addComma) out.write(", ")
        objectKeyRenderer
      }
      def visitKeyValue(s: Any): Unit = {
        out.write(" = ")
      }
      def visitValue(v: StringBuilderWriter, index: Int): Unit = {
        addComma = true
      }
      def visitEnd(index: Int): StringBuilderWriter = {
        addComma = false
        depth -= 1
        out.write(" }")
        flush
      }
    }
}

object TomlRenderer {
  @inline private def isBareKeyChar(c: Char): Boolean =
    (c >= 'A' && c <= 'Z') ||
    (c >= 'a' && c <= 'z') ||
    (c >= '0' && c <= '9') ||
    c == '_' ||
    c == '-'

  private def isBareKey(key: CharSequence): Boolean = {
    val len = key.length
    if (len == 0) false
    else {
      var i = 0
      while (i < len) {
        if (!isBareKeyChar(key.charAt(i))) return false
        i += 1
      }
      true
    }
  }

  def writeEscapedKey(out: StringBuilderWriter, key: CharSequence): Unit = {
    if (isBareKey(key)) out.write(key.toString)
    else BaseRenderer.escape(out, key, unicode = true)
  }

  def escapeKey(key: String): String = if (isBareKey(key)) key
  else {
    val out = new StringBuilderWriter()
    writeEscapedKey(out, key)
    out.toString
  }
}
