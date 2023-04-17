package sjsonnet

import java.io.{StringWriter, Writer}
import upickle.core.{ArrVisitor, ObjVisitor}

import scala.collection.mutable
/**
 * A version of YamlRenderer that tries its best to make the output YAML as
 * pretty as possible: unquoted strings, de-dented lists, etc. Follows the PyYAML
 * style. Also adds the ability to stream writes to a generic `Writer`.
 */
class PrettyYamlRenderer(out: Writer = new java.io.StringWriter(),
                         indentArrayInObject: Boolean = false,
                         indent: Int,
                         idealWidth: Int = 80,
                         getCurrentPosition: () => Position) extends BaseCharRenderer[Writer](out, indent){
  var newlineBuffered = false
  var dashBuffered = false
  var afterColon = false
  var afterKey = false
  var topLevel = true
  var leftHandPrefixOffset = 0
  var firstElementInArray = false
  var bufferedComment: String = null
  override def visitString(s: CharSequence, index: Int) = {
    addSpaceAfterColon()
    flushBuffer()

    // Although we can render strings in a multitude of different ways, here
    // we try our best to match PyYAML's style. Their style generally looks
    // pretty reasonable, and allows consistency with other tooling a significant
    // fraction of which is probably Python
    val str = s.toString

    // empty strings and single-newline strings are special-cased
    if (str.isEmpty) {
      out.append("''")
      saveCurrentPos()
    }
    else if (str == "\n") {
      out.append("|2+")
      saveCurrentPos()
      if (bufferedComment != null) out.append(bufferedComment)
      bufferedComment = null
      out.append("\n")
    }
    // Strings with trailing spaces or with unicode characters are written double-quoted
    else if (str.contains(" \n") || str.exists(_ > '~')) {
      PrettyYamlRenderer.writeDoubleQuoted(out, indent * (depth + 1), leftHandPrefixOffset, idealWidth, str)
      saveCurrentPos()
    }
    // Other strings with newlines are rendered as blocks
    else if (str.contains('\n')) {
      saveCurrentPos()
      PrettyYamlRenderer.writeBlockString(
        str,
        out,
        depth,
        indent,
        if (bufferedComment == null) "" else bufferedComment
      )
      bufferedComment = null
    }
    // Strings which look like booleans/nulls/numbers/dates/etc.,
    // or have leading/trailing spaces, are rendered single-quoted
    else if (PrettyYamlRenderer.stringNeedsToBeQuoted(str)) {
      val strWriter = new StringWriter
      BaseCharRenderer.escape(strWriter, s, unicode = true)
      val quotedStr = "'" + str.replace("'", "''") + "'"
      PrettyYamlRenderer.writeWrappedString(quotedStr, leftHandPrefixOffset, out, indent * (depth + 1), idealWidth)
      leftHandPrefixOffset = quotedStr.length + 2
      saveCurrentPos()
    } else { // All other strings can be rendered naked without quotes
      PrettyYamlRenderer.writeWrappedString(str, leftHandPrefixOffset, out, indent * (depth + 1), idealWidth)
      leftHandPrefixOffset = s.length
      saveCurrentPos()
    }
    out
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
    saveCurrentPos()
    out
  }

  val loadedFileContents = mutable.HashMap.empty[Path, Array[Int]]
  def saveCurrentPos() = {
    val current = getCurrentPosition()
    if (current != null){
      bufferedComment = " # " + current.currentFile.renderOffsetStr(current.offset, loadedFileContents)
    }
  }
  override def visitTrue(index: Int) = {
    addSpaceAfterColon()
    val out = super.visitTrue(index)
    saveCurrentPos()
    out
  }

  override def visitFalse(index: Int) = {
    addSpaceAfterColon()
    val out = super.visitFalse(index)
    saveCurrentPos()
    out
  }

  override def visitNull(index: Int) = {
    addSpaceAfterColon()
    val out = super.visitNull(index)
    saveCurrentPos()
    out
  }
  override def flushBuffer() = {
    if (newlineBuffered) {
      afterColon = false
      if (bufferedComment != null){
        out.append(bufferedComment)
        bufferedComment = null
      }
      PrettyYamlRenderer.writeIndentation(out, indent * depth)
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
        saveCurrentPos()
      }
      newlineBuffered = false
      dashBuffered = false
      out
    }
  }
  override def visitObject(length: Int, jsonKeys: Boolean, index: Int) = new ObjVisitor[Writer, Writer] {
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
      saveCurrentPos()
      if (bufferedComment != null){
        out.append(bufferedComment)
        bufferedComment = null
      }
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
        saveCurrentPos()
      }
      newlineBuffered = false
      depth -= 1
      flushBuffer()
      out
    }
  }
}


object PrettyYamlRenderer{


  /**
   * Renders a multi-line string with all indentation and whitespace preserved
   */
  def writeBlockString(str: String, out: Writer, depth: Int, indent: Int, lineComment: String) = {
    val len = str.length()
    val splits = YamlRenderer.newlinePattern.split(str, -1)
    val blockOffsetNumeral = if (str.charAt(0) != ' ') "" else indent
    val (blockStyle, dropRight) =
      (str.charAt(len - 1), if (len > 2) Some(str.charAt(len - 2)) else None) match{
        case ('\n', Some('\n')) => (s"|$blockOffsetNumeral+", 1)
        case ('\n', _) => (s"|$blockOffsetNumeral", 1)
        case (_, _) => (s"|$blockOffsetNumeral-", 0)
      }

    out.append(blockStyle)
    out.append(lineComment)

    splits.dropRight(dropRight).foreach { split =>
      if (split.nonEmpty) PrettyYamlRenderer.writeIndentation(out, indent * (depth + 1))
      else out.write('\n')
      out.append(split)
    }
  }


  /**
   * Wrap a double-quoted string. This behaves very differently from
   * [[writeWrappedString]], as it allows newline characters to be present
   * (escaped as `\n`), allows wrapping in the middle of a multi-' ' gap,
   * and requires `\`-escaping of line ends and starts.
   *
   * Transcribed directly from PyYAML implementation to get all the nuances right
   * https://github.com/yaml/pyyaml/blob/master/lib/yaml/emitter.py#L915-L985
   */
  def writeDoubleQuoted(out: Writer,
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

    def isBreakableChar(ch: Char, allowUnicode: Boolean) = {
      ch match{
        case '\"' | '\\' | '\u0085' | '\u2028' | '\u2029' | '\uFEFF' => true
        case _ =>
          val isNormalChar = '\u0020' <= ch && ch <= '\u007E'
          val isUnicodePrintableChar = '\u00A0' <= ch && ch <= '\uD7FF' || '\uE000' <= ch && ch <= '\uFFFD'
          !(isNormalChar || (allowUnicode && isUnicodePrintableChar))
      }
    }

    def getEscapeSequenceForChar(ch: Char): String = (ch: Char) match{
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '\"' => "\\\""
      case '\\' => "\\\\"
      case _ =>
        if(ch <= '\u00FF'){
          "\\u" + hex((ch >> 4) & 15) + hex(ch & 15)
        }else if(ch <= '\uFFFF'){
          "\\u" + hex((ch >> 12) & 15) + hex((ch >> 8) & 15) + hex((ch >> 4) & 15) + hex(ch & 15)
        } else ???
    }

    while (end <= text.length){
      val ch: Character = if (end < text.length) text(end) else null
      if (ch == null || isBreakableChar(ch, allowUnicode)){
        if (start < end){
          writeData(text.slice(start, end))
          start = end
        }
        if (ch != null){
          writeData(getEscapeSequenceForChar(ch))
          start = end+1
        }
      }
      if (0 < end && end < text.length -1 && (ch == ' ' || start >= end)
        && column+(end-start) > idealWidth && split){
        writeData(text.slice(start, end) + '\\')
        if (start < end) start = end
        if (column > idealWidth){
          PrettyYamlRenderer.writeIndentation(out, leftIndent)
          column = leftIndent
        }

        if (text(start) == ' ') writeData("\\")
      }
      end += 1
    }
    out.write('"')
  }

  private def hex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar.toUpper

  /**
   * Wraps a string by breaking it up into space-separated tokens, and appending
   * each token onto the string until it overshoots the `idealWidth` before wrapping.
   * Assumes there are no `\n` characters in the string to begin with.
   *
   * Is used for both naked and single quoted strings.
   */
  def writeWrappedString(s: String, leftHandPrefixOffset: Int, out: Writer, leftIndent: Int, idealWidth: Int) = {

    val tokens0 = s.split(" ", -1)
    // Consolidate tokens which are separated by more than 1 space, as these
    // cannot be wrapped across multiple lines since a newline character is
    // equivalent to a single space
    val tokens = collection.mutable.Buffer.empty[String]
    for(chunk <- tokens0){
      (tokens.lastOption, chunk) match{
        case (None, "") => tokens.append(" ")
        case (None, v) => tokens.append(v)
        case (Some(prev), "") => tokens(tokens.length-1) += " "
        case (Some(prev), v) =>
          if (prev.endsWith(" ")) tokens(tokens.length-1) += " " + v
          else tokens.append(v)
      }
    }

    var currentOffset = leftHandPrefixOffset + leftIndent
    var firstInLine = true
    var firstLine = true

    for(token <- tokens) {
      // This logic doesn't actually ensure that the text is wrapped to fit within
      // `idealWidth` characters width, but instead follows the behavior of the common PyYAML
      // library. Thus it is expected for the wrapped text to over-shoot the 80
      // character mark by up to one token, which can be of varying width
      val maxWidth = idealWidth
      if (!firstInLine && currentOffset > maxWidth){
        PrettyYamlRenderer.writeIndentation(out, leftIndent)
        firstLine = false
        currentOffset = leftIndent
        out.write(token)
      }else{
        if (firstInLine) firstInLine = false
        else {
          out.write(" ")
          currentOffset += 1
        }
        out.write(token)
      }
      currentOffset += token.length

    }
  }
  /**
   * Parses a string to check if it matches a YAML non-string syntax, in which
   * case it needs to be quoted when rendered. It's a pretty involved computation
   * to check for booleans/numbers/nulls/dates/collections/etc., so we use
   * FastParse to do it in a reasonably manageable and performant manner.
   */
  def stringNeedsToBeQuoted(str: String) = {
    import fastparse._
    import NoWhitespace._
    def yamlPunctuation[_: P] = P(
      // http://blogs.perl.org/users/tinita/2018/03/strings-in-yaml---to-quote-or-not-to-quote.html
      StringIn(
        "!", // ! Tag like !!null
        "&", // & Anchor like &mapping_for_later_use
        "*", // * Alias like *mapping_for_later_use
        "- ", // -<space> Block sequence entry
        ": ", // :<space> Block mapping entry
        "? ", // ?<space> Explicit mapping key
        "{", "}", "[", "]", // {, }, [, ] Flow mapping or sequence
        ",", // , Flow Collection entry seperator
        "#", // # Comment
        "|", ">", // |, > Block Scalar
        "@", "`", // @, '`' (backtick) Reserved characters
        "\"", "'", // ", ' Double and single quote
        " " // leading or trailing empty spaces need quotes to define them
      )
    )
    def yamlKeyword[_: P] = P(
      StringIn(
        // https://makandracards.com/makandra/24809-yaml-keys-like-yes-or-no-evaluate-to-true-and-false
        // y|Y|yes|Yes|YES|n|N|no|No|NO
        // |true|True|TRUE|false|False|FALSE
        // |on|On|ON|off|Off|OFF
        "yes", "Yes", "YES", "no", "No", "NO",
        "true", "True", "TRUE", "false", "False", "FALSE",
        "on", "On", "ON", "off", "Off", "OFF",
        "null", "Null", "NULL", "~",
        // Somehow PyYAML doesn't count the single-letter booleans as things
        // that need to be quoted, so we don't count them either
        /*"y", "Y", "n", "N", */
        "-", "=" // Following PyYAML implementation, which quotes these even though it's not necessary
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
      // Use a `&` lookahead to bail out early in the common case, so we don't
      // need to try parsing times/dates/numbers one by one
      yamlPunctuation | (&(CharIn(".0-9\\-")) ~ (yamlTime | yamlDate | yamlNumber) | yamlKeyword) ~ End
    )

    fastparse.parse(str, parser(_)).isSuccess ||
    str.contains(": ") || // Looks like a key-value pair
    str.contains(" #") || // Comments
    str.charAt(str.length - 1) == ':' || // Looks like a key-value pair
    str.charAt(str.length - 1) == ' ' // trailing space needs quotes
  }

  def writeIndentation(out: Writer, n: Int) = {
    out.append('\n')
    var i = n
    while(i > 0) {
      out.append(' ')
      i -= 1
    }
  }

}
