package sjsonnet
import java.io.{StringWriter, Writer}
import java.util.regex.Pattern

import upickle.core.{ArrVisitor, ObjVisitor}
import ujson.BaseRenderer

/**
  * Custom JSON renderer to try and match the behavior of google/jsonnet's
  * render:
  *
  * - Custom printing of Doubles
  * - Custom printing of empty dictionaries and arrays
  *
  */
class Renderer(out: Writer = new java.io.StringWriter(),
               indent: Int = -1) extends BaseRenderer(out, indent){
  var newlineBuffered = false
  override def visitFloat64(d: Double, index: Int) = {
    flushBuffer()
    out.append(RenderUtils.renderDouble(d))
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
  override def visitArray(length: Int, index: Int) = new ArrVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    out.append('[')
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

      if (empty) out.append(' ')
      else renderIndent()
      out.append(']')
      out
    }
  }

  override def visitObject(length: Int, index: Int) = new ObjVisitor[Writer, Writer] {
    var empty = true
    flushBuffer()
    out.append('{')
    newlineBuffered = true
    depth += 1
    def subVisitor = Renderer.this
    def visitKey(index: Int) = Renderer.this
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
                         indent: Int = 2) extends BaseRenderer[Writer](out, indent){
  var newlineBuffered = false
  var dashBuffered = false
  var afterColon = false
  var afterKey = false
  var topLevel = true
  var leftHandPrefixOffset = 0
  var firstElementInArray = false
  val newlinePattern = Pattern.compile("\n")

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar.toUpper

  override def visitString(s: CharSequence, index: Int) = {

    addSpaceAfterColon()
    flushBuffer()
    val len = s.length()
    if (len == 0) out.append("''")
    else if (s.toString == "\n") out.append("|2+\n")
    else if (s.toString.contains(" \n") || s.toString.exists(c => c > '~')){
      write_double_quoted(s.toString)
      out
    } else if (s.toString.contains('\n')){
      val splits = newlinePattern.split(s, -1)
      val blockOffsetNumeral = s.toString.takeWhile(_ == ' ').size match{case 0 => ""; case n => indent}
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
    } else if (needsToBeQuoted(s)) {
      val str = new StringWriter
      ujson.Renderer.escape(str, s, unicode = true)
      val quotedStr = "'" + s.toString.replace("'", "''") + "'"
      wrapString(quotedStr)
      leftHandPrefixOffset = quotedStr.length + 2
      out
    } else {
      wrapString(s.toString)
      leftHandPrefixOffset = s.length
      out
    }
  }
  def needsToBeQuoted(s: CharSequence) = {
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
    def yamlNumber[_: P] = P( &(CharIn(".0-9\\-")) ~ "-".? ~ yamlNumber0 )

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
      yamlPunctuation | (yamlTime | yamlDate | yamlNumber | yamlKeyword) ~ End
    )

    val str = s.toString
    fastparse.parse(str, parser(_)).isSuccess ||
    str.contains(": ") ||
    str.contains(" #") ||
    str.charAt(str.length - 1) == ':' ||
    str.charAt(str.length - 1) == ' '
  }

  // Transcribed directly from PyYAML implementation
  // https://github.com/yaml/pyyaml/blob/master/lib/yaml/emitter.py#L915-L985
  val ESCAPE_REPLACEMENTS = Map(
    '\0' ->     "0",
    '\b' ->    "b",
    '\t' ->    "t",
    '\n' ->    "n",
    '\f' ->    "f",
    '\r' ->    "r",
    '\"' ->     "\"",
    '\\' ->     "\\",
  )

  def write_double_quoted(text: String, split: Boolean = true, allowUnicode: Boolean = false) = {
    out.write('"')
    //      self.write_indicator('"', true)
    var column = leftHandPrefixOffset + indent * (depth + 1) + 1 // +1 to include the open quote
    var start = 0
    var end = 0
    while (end <= text.length){
      var ch: Character = null
      if (end < text.length) {
        ch = text(end)
      }
      if (
        ch == null ||
        "\"\\\u0085\u2028\u2029\uFEFF".contains(ch)  ||
        !('\u0020' <= ch && ch <= '\u007E' ||
          (allowUnicode && ('\u00A0' <= ch && ch <= '\uD7FF' || '\uE000' <= ch && ch <= '\uFFFD'))
        )
      ){

        if (start < end){
          val data = text.slice(start, end)
          column += data.length
          out.write(data)
          start = end
        }
        if (ch != null){
          var data = ""
          if (ESCAPE_REPLACEMENTS.contains(ch)){
            data = "\\"+ESCAPE_REPLACEMENTS(ch)
          }else if(ch <= '\u00FF'){
            data = "\\u" + toHex((ch >> 12) & 15) + toHex((ch >> 8) & 15) + toHex((ch >> 4) & 15) + toHex(ch & 15)
          }else if(ch <= '\uFFFF'){
            data = "\\u" + toHex((ch >> 12) & 15) + toHex((ch >> 8) & 15) + toHex((ch >> 4) & 15) + toHex(ch & 15)
          }else{
            ???
          }
          column += data.length
          out.write(data)
          start = end+1
        }
      }
      if (0 < end && end < text.length -1 && (ch == ' ' || start >= end)
        && column+(end-start) > 80 && split){
        val data = text.slice(start, end) + '\\'

        if (start < end){
          start = end
        }
        column += data.length
        out.write(data)
        if (column > 80){

          out.write("\n")
          out.write(" " * (indent * (depth + 1)))
          column = indent * (depth + 1)
        }

        //              self.write_indent()

        if (text(start) == ' ') {
          val data = "\\"
          column += data.length

          out.write(data)
        }
      }
      end += 1
    }
    out.write('"')
  }

  def wrapString(s: String) = {

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

    var currentOffset = leftHandPrefixOffset + indent * (depth + 1)
    var firstInLine = true
    var firstLine = true

    for(chunk <- chunks) {
      // This logic doesn't actually ensure that the text is wrapped to fit within
      // 80 characters width, but instead follows the behavior of the common PyYAML
      // library. Thus it is expected for the wrapped text to over-shoot the 80
      // character mark by up to one token, which can be of varying width
      val maxWidth = 80
      if (!firstInLine && currentOffset > maxWidth){
        out.write("\n")
        firstLine = false

        out.write(" " * (indent * (depth + 1)))

        currentOffset = indent * (depth + 1)

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


class PythonRenderer(out: Writer = new java.io.StringWriter(),
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
