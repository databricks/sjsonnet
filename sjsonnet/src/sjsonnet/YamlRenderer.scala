package sjsonnet

import java.io.{StringWriter, Writer}
import java.util.regex.Pattern

import upickle.core.{ArrVisitor, ObjVisitor}
import ujson.BaseRenderer



class YamlRenderer(out: StringWriter = new java.io.StringWriter(), indentArrayInObject: Boolean = false,
                   indent: Int = 2) extends BaseRenderer(out, indent){
  var newlineBuffered = false
  var dashBuffered = false
  var afterKey = false
  var topLevel = true

  val newlinePattern = Pattern.compile("\n")
  val outBuffer = out.getBuffer()

  override def visitString(s: CharSequence, index: Int): StringWriter = {
    flushBuffer()
    val len = s.length()
    if (len == 0) out.append("\"\"")
    else if (s.charAt(len - 1) == '\n') {
      val splits = newlinePattern.split(s)
      out.append('|')
      depth += 1
      splits.foreach { split =>
        newlineBuffered = true
        flushBuffer()
        out.append(split) // TODO escaping?
      }
      depth -= 1
      out
    } else {
      ujson.Renderer.escape(out, s, unicode = true)
      out
    }
  }
  override def visitFloat64(d: Double, index: Int) = {
    flushBuffer()
    out.append(RenderUtils.renderDouble(d))
    out
  }
  override val colonSnippet = ": "
  override def flushBuffer() = {
    if (newlineBuffered) {
      // drop space between colon and newline
      if (outBuffer.length() > 1 && outBuffer.charAt(outBuffer.length() - 1) == ' ') {
        outBuffer.setLength(outBuffer.length() - 1)
      }
      out.append('\n')

      var i = indent * depth
      while(i > 0) {
        out.append(' ')
        i -= 1
      }
    }
    if (dashBuffered) {
      out.append("- ")
    }
    dashBuffered = false
    newlineBuffered = false
    dashBuffered = false
  }
  override def visitArray(length: Int, index: Int) = new ArrVisitor[StringWriter, StringWriter] {
    var empty = true
    flushBuffer()

    if (!topLevel) {
      depth += 1
      newlineBuffered = true
    }
    topLevel = false

    var dedentInObject = afterKey && !indentArrayInObject
    afterKey = false
    if (dedentInObject) depth -= 1
    dashBuffered = true

    def subVisitor = YamlRenderer.this
    def visitValue(v: StringWriter, index: Int): Unit = {
      empty = false
      flushBuffer()
      newlineBuffered = true
      dashBuffered = true
    }
    def visitEnd(index: Int) = {
      if (!dedentInObject) depth -= 1
      if (empty) out.append("[]")
      newlineBuffered = false
      dashBuffered = false
      out
    }
  }
  override def visitObject(length: Int, index: Int) = new ObjVisitor[StringWriter, StringWriter] {
    var empty = true
    flushBuffer()
    if (!topLevel) depth += 1
    topLevel = false

    if (afterKey) newlineBuffered = true

    def subVisitor = YamlRenderer.this
    def visitKey(index: Int) = YamlRenderer.this
    def visitKeyValue(s: Any): Unit = {
      empty = false
      flushBuffer()
      out.append(colonSnippet)
      afterKey = true
      newlineBuffered = false
    }
    def visitValue(v: StringWriter, index: Int): Unit = {
      newlineBuffered = true
      afterKey = false
    }
    def visitEnd(index: Int) = {
      if (empty) out.append("{}")
      newlineBuffered = false
      depth -= 1
      flushBuffer()
      out
    }
  }
}

/**
 * A version of YamlRenderer that tries its best to make the output YAML as
 * pretty as possible: unquoted strings, de-dented lists, etc.
 */
class PrettyYamlRenderer(out: Writer = new java.io.StringWriter(),
                         indentArrayInObject: Boolean = false,
                         indent: Int = 2,
                         idealWidth: Int = 80) extends BaseRenderer[Writer](out, indent){
  var newlineBuffered = false
  var dashBuffered = false
  var afterColon = false
  var afterKey = false
  var topLevel = true
  var leftHandPrefixOffset = 0
  var firstElementInArray = false
  val newlinePattern = Pattern.compile("\n")

  override def visitString(s: CharSequence, index: Int) = {

    addSpaceAfterColon()
    flushBuffer()
    val len = s.length()
    val str = s.toString
    if (len == 0) out.append("''")
    else if (str == "\n") out.append("|2+\n")
    else if (str.contains(" \n") || str.exists(c => c > '~')){
      PrettyYamlRenderer.write_double_quoted(out, indent * (depth + 1), leftHandPrefixOffset, idealWidth, str)
      out
    } else if (str.contains('\n')){
      val splits = newlinePattern.split(s, -1)
      val blockOffsetNumeral = str.takeWhile(_ == ' ').size match{case 0 => ""; case n => indent}
      val (blockStyle, dropRight) = (s.charAt(len - 1), if (len > 2) Some(s.charAt(len - 2)) else None) match{
        case ('\n', Some('\n')) => (s"|$blockOffsetNumeral+", 1)
        case ('\n', _) => (s"|$blockOffsetNumeral", 1)
        case (_, _) => (s"|$blockOffsetNumeral-", 0)
      }

      out.append(blockStyle)

      depth += 1
      splits.dropRight(dropRight).foreach { split =>
        newlineBuffered = true
        val oldDepth = depth
        if (split == "") depth = 0
        flushBuffer()
        depth = oldDepth
        out.append(split) // TODO escaping?
      }
      depth -= 1
      out
    } else if (PrettyYamlRenderer.stringNeedsToBeQuoted(str)) {
      val strWriter = new StringWriter
      ujson.Renderer.escape(strWriter, s, unicode = true)
      val quotedStr = "'" + str.replace("'", "''") + "'"
      PrettyYamlRenderer.wrapString(quotedStr, leftHandPrefixOffset, out, indent * (depth + 1), idealWidth)
      leftHandPrefixOffset = quotedStr.length + 2
      out
    } else {
      PrettyYamlRenderer.wrapString(str, leftHandPrefixOffset, out, indent * (depth + 1), idealWidth)
      leftHandPrefixOffset = s.length
      out
    }
  }

  def addSpaceAfterColon() = {
    if (afterColon) {
      out.append(' ')
      afterColon = false
    }
  }
  override def visitFloat64(d: Double, index: Int) = {
    addSpaceAfterColon()
    flushBuffer()
    out.append(RenderUtils.renderDouble(d))
    out
  }

  override def visitTrue(index: Int) = {
    addSpaceAfterColon()
    super.visitTrue(index)
  }

  override def visitFalse(index: Int) = {
    addSpaceAfterColon()
    super.visitFalse(index)
  }

  override def visitNull(index: Int) = {
    addSpaceAfterColon()
    super.visitNull(index)
  }
  override def flushBuffer() = {
    if (newlineBuffered) {
      afterColon = false
      out.append('\n')

      var i = indent * depth
      while(i > 0) {
        out.append(' ')
        i -= 1
      }
    }
    if (dashBuffered) {
      out.append("- ")
    }
    dashBuffered = false
    newlineBuffered = false
    dashBuffered = false
  }
  override def visitArray(length: Int, index: Int) = new ArrVisitor[Writer, Writer] {
    var empty = true
    val dedentInObject = afterKey && !indentArrayInObject
    def subVisitor = {
      if (empty){
        afterColon = false
        flushBuffer()
        val outerFirstElementInArray = firstElementInArray
        firstElementInArray = true
        if (!topLevel) {
          depth += 1
          if (!firstElementInArray || !outerFirstElementInArray)  newlineBuffered = true
        }
        topLevel = false


        afterKey = false
        if (dedentInObject) depth -= 1
        dashBuffered = true
        empty = false
      }
      leftHandPrefixOffset = 0
      PrettyYamlRenderer.this
    }
    def visitValue(v: Writer, index: Int): Unit = {
      firstElementInArray = true
      empty = false
      flushBuffer()
      newlineBuffered = true

      dashBuffered = true
    }
    def visitEnd(index: Int) = {
      firstElementInArray = false
      if (!dedentInObject) depth -= 1
      if (empty) {
        addSpaceAfterColon()
        out.append("[]")
      }
      newlineBuffered = false
      dashBuffered = false
      out
    }
  }
  override def visitObject(length: Int, index: Int) = new ObjVisitor[Writer, Writer] {
    firstElementInArray = false
    var empty = true
    flushBuffer()
    if (!topLevel) depth += 1
    topLevel = false
    def subVisitor = PrettyYamlRenderer.this
    def visitKey(index: Int) = {
      if (empty){
        leftHandPrefixOffset = 0

        afterColon = false
        if (afterKey) newlineBuffered = true
        empty = false
      }
      PrettyYamlRenderer.this
    }
    def visitKeyValue(s: Any): Unit = {
      empty = false
      flushBuffer()
      out.append(":")
      afterKey = true
      afterColon = true
      newlineBuffered = false
    }
    def visitValue(v: Writer, index: Int): Unit = {
      newlineBuffered = true
      afterKey = false
    }
    def visitEnd(index: Int) = {
      if (empty) {
        addSpaceAfterColon()
        out.append("{}")
      }
      newlineBuffered = false
      depth -= 1
      flushBuffer()
      out
    }
  }
}


object PrettyYamlRenderer{
  def stringNeedsToBeQuoted(str: String) = {
    import fastparse._, NoWhitespace._
    def yamlPunctuation[_: P] = P(
      // http://blogs.perl.org/users/tinita/2018/03/strings-in-yaml---to-quote-or-not-to-quote.html
      StringIn(
        "!", // ! Tag like !!null
        "&", // & Anchor like &mapping_for_later_use
        "*", // * Alias like *mapping_for_later_use
        "- ", // -<space> Block sequence entry
        ": ", // :<space> Block mapping entry
        "? ", // ?<space> Explicit mapping key
        "{", // {, }, [, ] Flow mapping or sequence
        "}",
        "[",
        "]",
        ",", // , Flow Collection entry seperator
        "#", // # Comment
        "|", // |, > Block Scalar
        ">",
        "@", // @, '`' (backtick) Reserved characters
        "`",
        "\"", // ", ' Double and single quote
        "'",
        " " // leading or trailing empty spaces need quotes to define them
      )
    )
    def yamlKeyword[_: P] = P(
      StringIn(
        // https://makandracards.com/makandra/24809-yaml-keys-like-yes-or-no-evaluate-to-true-and-false
        // y|Y|yes|Yes|YES|n|N|no|No|NO
        // |true|True|TRUE|false|False|FALSE
        // |on|On|ON|off|Off|OFF

        // Somehow PyYAML doesn't count the single-letter booleans as things
        // that need to be quoted, so we don't count them either
        /*"y", "Y", */"yes", "Yes", "YES",
        /*"n", "N", */"no", "No", "NO",
        "true", "True", "TRUE",
        "false", "False", "FALSE",
        "on", "On", "ON",
        "off", "Off", "OFF",
        "null", "Null", "NULL", "~",

        "-", "=" // Following PyYAML implementation, which quotes these for some reason
      )
    )


    def digits[_: P] = P( CharsWhileIn("0-9") )
    def yamlFloat[_: P] = P(
      (digits.? ~ "." ~ digits | digits ~ ".") ~ (("e" | "E") ~ ("+" | "-").? ~ digits).?
    )
    def yamlOctalSuffix[_: P] = P( "x" ~ CharIn("1-9a-fA-F") ~ CharsWhileIn("0-9a-fA-F").? )
    def yamlHexSuffix[_: P] = P( "o".? ~ CharIn("1-7") ~ CharsWhileIn("0-7").? )
    def yamlOctalHex[_: P] = P( "0" ~ (yamlOctalSuffix | yamlHexSuffix) )
    def yamlNumber0[_: P] = P( ".inf" | yamlFloat | yamlOctalHex | CharIn("1-9") ~ digits.? | "0" )

    // Add a `CharIn` lookahead to bail out quickly if something cannot possibly be a number
    def yamlNumber[_: P] = P( "-".? ~ yamlNumber0 )

    // Strings and numbers aren't the only scalars that YAML can understand.
    // ISO-formatted date and datetime literals are also parsed.
    // date:                  2002-12-14
    // datetime:              2001-12-15T02:59:43.1Z
    // datetime_with_spaces:  2001-12-14 21:59:43.10 -5

    def fourDigits[_: P] = P( CharIn("0-9") ~ CharIn("0-9") ~ CharIn("0-9") ~ CharIn("0-9") )
    def oneTwoDigits[_: P] = P( CharIn("0-9") ~ CharIn("0-9").? )
    def twoDigits[_: P] = P( CharIn("0-9") ~ CharIn("0-9") )
    def dateTimeSuffix[_: P] = P(
      ("T" | " ") ~ twoDigits ~ ":" ~ twoDigits ~ ":" ~ twoDigits ~
        ("." ~ digits.?).? ~ ((" " | "Z").? ~ ("-".? ~ oneTwoDigits).?).?
    )
    def yamlDate[_: P] = P( fourDigits ~ "-" ~ oneTwoDigits ~ "-" ~ oneTwoDigits ~ dateTimeSuffix.? )

    // Not in the YAML, but included to match PyYAML behavior
    def yamlTime[_: P] = P( twoDigits ~ ":" ~ twoDigits )

    def parser[_: P] = P(
      yamlPunctuation | (&(CharIn(".0-9\\-")) ~ (yamlTime | yamlDate | yamlNumber) | yamlKeyword) ~ End
    )

    fastparse.parse(str, parser(_)).isSuccess ||
      str.contains(": ") ||
      str.contains(" #") ||
      str.charAt(str.length - 1) == ':' ||
      str.charAt(str.length - 1) == ' '
  }



  def isBreakableChar(ch: Char, allowUnicode: Boolean) = {
    ch match{
      case '\"' => true
      case '\\' => true
      case '\u0085' => true
      case '\u2028' => true
      case '\u2029' => true
      case '\uFEFF' => true
      case _ =>
        val isNormalChar = '\u0020' <= ch && ch <= '\u007E'
        val isUnicodePrintableChar = '\u00A0' <= ch && ch <= '\uD7FF' || '\uE000' <= ch && ch <= '\uFFFD'
        !(isNormalChar || (allowUnicode && isUnicodePrintableChar))
    }
  }

  // Transcribed directly from PyYAML implementation
  // https://github.com/yaml/pyyaml/blob/master/lib/yaml/emitter.py#L915-L985
  def write_double_quoted(out: Writer,
                          leftIndent: Int,
                          leftHandPrefixOffset: Int,
                          idealWidth: Int,
                          text: String,
                          split: Boolean = true,
                          allowUnicode: Boolean = false) = {
    out.write('"')
    var column = leftHandPrefixOffset + leftIndent + 1 // +1 to include the open quote
    var start = 0
    var end = 0
    def writeData(data: String) = {
      out.write(data)
      column += data.length
    }
    while (end <= text.length){
      var ch: Character = null
      if (end < text.length) ch = text(end)
      if (ch == null || isBreakableChar(ch, allowUnicode)){
        if (start < end){
          writeData(text.slice(start, end))
          start = end
        }
        if (ch != null){
          val data = (ch: Char) match{
            case '\0' => "\\0"
            case '\b' => "\\b"
            case '\t' => "\\t"
            case '\n' => "\\n"
            case '\f' => "\\f"
            case '\r' => "\\r"
            case '\"' => "\\\""
            case '\\' => "\\\\"
            case _ =>
              if(ch <= '\u00FF'){
                "\\u" + toHex((ch >> 4) & 15) + toHex(ch & 15)
              }else if(ch <= '\uFFFF'){
                "\\u" + toHex((ch >> 12) & 15) + toHex((ch >> 8) & 15) + toHex((ch >> 4) & 15) + toHex(ch & 15)
              }else{
                ???
              }
          }
          writeData(data)
          start = end+1
        }
      }
      if (0 < end && end < text.length -1 && (ch == ' ' || start >= end)
        && column+(end-start) > idealWidth && split){
        val data = text.slice(start, end) + '\\'

        if (start < end) start = end
        writeData(data)
        if (column > idealWidth){

          out.write("\n")
          out.write(" " * leftIndent)
          column = leftIndent
        }

        if (text(start) == ' ') writeData("\\")
      }
      end += 1
    }
    out.write('"')
  }

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar.toUpper

  def wrapString(s: String, leftHandPrefixOffset: Int, out: Writer, leftIndent: Int, idealWidth: Int) = {

    val chunks = collection.mutable.Buffer.empty[String]
    for(chunk <- s.split(" ", -1)){
      (chunks.lastOption, chunk) match{
        case (None, "") => chunks.append(" ")
        case (None, v) => chunks.append(v)
        case (Some(prev), "") => chunks(chunks.length-1) += " "
        case (Some(prev), v) =>
          if (prev.endsWith(" ")) chunks(chunks.length-1) += " " + v
          else chunks.append(v)
      }
    }

    var currentOffset = leftHandPrefixOffset + leftIndent
    var firstInLine = true
    var firstLine = true

    for(chunk <- chunks) {
      // This logic doesn't actually ensure that the text is wrapped to fit within
      // 80 characters width, but instead follows the behavior of the common PyYAML
      // library. Thus it is expected for the wrapped text to over-shoot the 80
      // character mark by up to one token, which can be of varying width
      val maxWidth = idealWidth
      if (!firstInLine && currentOffset > maxWidth){
        out.write("\n")
        firstLine = false

        out.write(" " * leftIndent)

        currentOffset = leftIndent

        out.write(chunk)

      }else{
        if (firstInLine) firstInLine = false
        else {
          out.write(" ")
          currentOffset += 1
        }
        out.write(chunk)
      }
      currentOffset += chunk.length

    }
  }

}