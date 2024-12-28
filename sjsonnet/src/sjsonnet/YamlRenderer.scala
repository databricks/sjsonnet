package sjsonnet

import java.io.StringWriter
import java.util.regex.Pattern
import upickle.core.{ArrVisitor, ObjVisitor, SimpleVisitor, Visitor}

import scala.util.Try



class YamlRenderer(_out: StringWriter = new java.io.StringWriter(), indentArrayInObject: Boolean = false,
                   quoteKeys: Boolean = true, indent: Int = 2) extends BaseCharRenderer(_out, indent){
  var newlineBuffered = false
  var dashBuffered = false
  var afterKey = false
  private var topLevel = true
  private val outBuffer = _out.getBuffer

  private val yamlKeyVisitor = new SimpleVisitor[StringWriter, StringWriter]() {
    override def expectedMsg = "Expected a string key"
    override def visitString(s: CharSequence, index: Int): StringWriter = {
      YamlRenderer.this.flushBuffer()
      if (quoteKeys || !YamlRenderer.isSafeBareKey(s.toString)) {
        upickle.core.RenderUtils.escapeChar(null, YamlRenderer.this.elemBuilder, s, escapeUnicode = true, wrapQuotes = true)
      } else {
        YamlRenderer.this.appendString(s.toString)
      }
      YamlRenderer.this.flushCharBuilder()
      _out
    }
  }

  override def flushCharBuilder(): Unit = {
    elemBuilder.writeOutToIfLongerThan(_out, if (depth <= 0 || topLevel) 0 else 1000)
  }

  private[this] def appendString(s: String): Unit = {
    val len = s.length
    var i = 0
    elemBuilder.ensureLength(len)
    while(i < len) {
      elemBuilder.appendUnsafeC(s.charAt(i))
      i += 1
    }
  }

  override def visitString(s: CharSequence, index: Int): StringWriter = {
    flushBuffer()
    val len = s.length()
    if (len == 0) {
      elemBuilder.ensureLength(2)
      elemBuilder.append('"')
      elemBuilder.append('"')
    } else if (s.charAt(len - 1) == '\n') {
      val splits = YamlRenderer.newlinePattern.split(s)
      elemBuilder.append('|')
      depth += 1
      splits.foreach { split =>
        newlineBuffered = true
        flushBuffer()
        appendString(split) // TODO escaping?
      }
      depth -= 1
    } else {
      upickle.core.RenderUtils.escapeChar(null, elemBuilder, s, escapeUnicode=true, wrapQuotes=true)
    }
    flushCharBuilder()
    _out
  }

  override def visitFloat64(d: Double, index: Int): StringWriter = {
    flushBuffer()
    appendString(RenderUtils.renderDouble(d))
    flushCharBuilder()
    _out
  }

  override def flushBuffer(): Unit = {
    if (newlineBuffered) {
      // drop space between colon and newline
      elemBuilder.writeOutToIfLongerThan(_out, 0)
      if (outBuffer.length() > 1 && outBuffer.charAt(outBuffer.length() - 1) == ' ') {
        outBuffer.setLength(outBuffer.length() - 1)
      }
      YamlRenderer.writeIndentation(elemBuilder, indent * depth)
      flushCharBuilder()
    }
    if (dashBuffered) {
      elemBuilder.append('-')
      elemBuilder.append(' ')
      flushCharBuilder()
    }
    dashBuffered = false
    newlineBuffered = false
    dashBuffered = false
  }

  override def visitArray(length: Int, index: Int): ArrVisitor[StringWriter, StringWriter] = new ArrVisitor[StringWriter, StringWriter] {
    var empty = true
    flushBuffer()

    if (!topLevel) {
      depth += 1
      newlineBuffered = true
    }
    topLevel = false

    private val dedentInObject = afterKey && !indentArrayInObject
    afterKey = false
    if (dedentInObject) depth -= 1
    dashBuffered = true

    def subVisitor: Visitor[StringWriter, StringWriter] = YamlRenderer.this
    def visitValue(v: StringWriter, index: Int): Unit = {
      empty = false
      flushBuffer()
      newlineBuffered = true
      dashBuffered = true
    }
    def visitEnd(index: Int): StringWriter = {
      if (!dedentInObject) depth -= 1
      if (empty) {
        elemBuilder.ensureLength(2)
        elemBuilder.append('[')
        elemBuilder.append(']')
      }
      newlineBuffered = false
      dashBuffered = false
      flushCharBuilder()
      _out
    }
  }

  override def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[StringWriter, StringWriter] = new ObjVisitor[StringWriter, StringWriter] {
    var empty = true
    flushBuffer()
    if (!topLevel) depth += 1
    topLevel = false

    if (afterKey) newlineBuffered = true

    def subVisitor: Visitor[StringWriter, StringWriter] = YamlRenderer.this

    def visitKey(index: Int): Visitor[StringWriter, StringWriter] = yamlKeyVisitor

    def visitKeyValue(s: Any): Unit = {
      empty = false
      flushBuffer()
      elemBuilder.ensureLength(2)
      elemBuilder.append(':')
      elemBuilder.append(' ')
      flushCharBuilder()
      afterKey = true
      newlineBuffered = false
    }

    def visitValue(v: StringWriter, index: Int): Unit = {
      newlineBuffered = true
      afterKey = false
    }

    def visitEnd(index: Int): StringWriter = {
      if (empty) {
        elemBuilder.ensureLength(2)
        elemBuilder.append('{')
        elemBuilder.append('}')
      }
      newlineBuffered = false
      depth -= 1
      flushCharBuilder()
      flushBuffer()
      _out
    }
  }
}
object YamlRenderer{
  val newlinePattern: Pattern = Pattern.compile("\n")
  private val safeYamlKeyPattern = Pattern.compile("^[a-zA-Z0-9/._-]+$")
  private val yamlReserved = Set("true", "false", "null", "yes", "no", "on", "off", "y", "n", ".nan",
    "+.inf", "-.inf", ".inf", "null", "-", "---", "''")
  private val yamlTimestampPattern = Pattern.compile("^(?:[0-9]*-){2}[0-9]*$")
  private val yamlBinaryPattern = Pattern.compile("^[-+]?0b[0-1_]+$")
  private val yamlHexPattern = Pattern.compile("[-+]?0x[0-9a-fA-F_]+")
  private val yamlFloatPattern = Pattern.compile( "^-?([0-9_]*)*(\\.[0-9_]*)?(e[-+][0-9_]+)?$" )
  private val yamlIntPattern = Pattern.compile("^[-+]?[0-9_]+$")

  private def isSafeBareKey(k: String) = {
    val l = k.toLowerCase
    !yamlReserved.contains(l) &&
      safeYamlKeyPattern.matcher(k).matches() &&
      !yamlTimestampPattern.matcher(l).matches() &&
      !yamlBinaryPattern.matcher(k).matches() &&
      !yamlHexPattern.matcher(k).matches() &&
      !yamlFloatPattern.matcher(l).matches() &&
      !yamlIntPattern.matcher(l).matches()
  }

  def writeIndentation(out: upickle.core.CharBuilder, n: Int): Unit = {
    out.ensureLength(n+1)
    out.append('\n')
    var i = n
    while(i > 0) {
      out.append(' ')
      i -= 1
    }
  }
}
