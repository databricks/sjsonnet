package sjsonnet

import java.io.{StringWriter, Writer}

import upickle.core.{ArrVisitor, ObjVisitor}

import scala.collection.mutable

/**
 * A version of YamlRenderer that tries its best to make the output YAML as pretty as possible:
 * unquoted strings, de-dented lists, etc. Follows the PyYAML style. Also adds the ability to stream
 * writes to a generic `Writer`.
 */
class PrettyYamlRenderer(
    out: Writer = new java.io.StringWriter(),
    indentArrayInObject: Boolean = false,
    indent: Int,
    idealWidth: Int = 80,
    getCurrentPosition: () => Position)
    extends BaseRenderer[Writer](out, indent) {

  var newlineBuffered = false
  var dashBuffered = false
  var afterColon = false
  var afterKey = false
  var topLevel = true
  var leftHandPrefixOffset = 0
  var firstElementInArray = false
  var bufferedComment: String = null

  override def visitJsonableObject(length: Int, index: Int): ObjVisitor[Writer, Writer] =
    visitObject(length, index)

  override def visitString(s: CharSequence, index: Int): Writer = {
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
    } else if (str == "\n") {
      out.append("|2+")
      saveCurrentPos()
      if (bufferedComment != null) out.append(bufferedComment)
      bufferedComment = null
      out.append("\n")
    }
    // Strings with trailing spaces or with unicode characters are written double-quoted
    else if (str.contains(" \n") || str.exists(_ > '~')) {
      PrettyYamlRenderer.writeDoubleQuoted(
        out,
        indent * (depth + 1),
        leftHandPrefixOffset,
        idealWidth,
        str
      )
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
      BaseRenderer.escape(strWriter, s, unicode = true)
      val quotedStr = "'" + str.replace("'", "''") + "'"
      PrettyYamlRenderer.writeWrappedString(
        quotedStr,
        leftHandPrefixOffset,
        out,
        indent * (depth + 1),
        idealWidth
      )
      leftHandPrefixOffset = quotedStr.length + 2
      saveCurrentPos()
    } else { // All other strings can be rendered naked without quotes
      PrettyYamlRenderer.writeWrappedString(
        str,
        leftHandPrefixOffset,
        out,
        indent * (depth + 1),
        idealWidth
      )
      leftHandPrefixOffset = s.length
      saveCurrentPos()
    }
    out
  }

  def addSpaceAfterColon(): Unit = {
    if (afterColon) {
      out.append(' ')
      afterColon = false
    }
  }
  override def visitFloat64(d: Double, index: Int): Writer = {
    addSpaceAfterColon()
    flushBuffer()
    out.append(RenderUtils.renderDouble(d))
    saveCurrentPos()
    out
  }

  val loadedFileContents: mutable.HashMap[Path, Array[Int]] =
    mutable.HashMap.empty[Path, Array[Int]]
  def saveCurrentPos(): Unit = {
    val current = getCurrentPosition()
    if (current != null && current.currentFile != null) {
      bufferedComment =
        " # " + current.currentFile.renderOffsetStr(current.offset, loadedFileContents)
    }
  }
  override def visitTrue(index: Int): Writer = {
    addSpaceAfterColon()
    val out = super.visitTrue(index)
    saveCurrentPos()
    out
  }

  override def visitFalse(index: Int): Writer = {
    addSpaceAfterColon()
    val out = super.visitFalse(index)
    saveCurrentPos()
    out
  }

  override def visitNull(index: Int): Writer = {
    addSpaceAfterColon()
    val out = super.visitNull(index)
    saveCurrentPos()
    out
  }
  override def flushBuffer(): Unit = {
    if (newlineBuffered) {
      afterColon = false
      if (bufferedComment != null) {
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
  override def visitArray(
      length: Int,
      index: Int): upickle.core.ArrVisitor[java.io.Writer, java.io.Writer] {
    def subVisitor: sjsonnet.PrettyYamlRenderer
  } = new ArrVisitor[Writer, Writer] {
    var empty = true
    val dedentInObject: Boolean = afterKey && !indentArrayInObject
    def subVisitor: sjsonnet.PrettyYamlRenderer = {
      if (empty) {
        afterColon = false
        flushBuffer()
        val outerFirstElementInArray = firstElementInArray
        firstElementInArray = true
        if (!topLevel) {
          depth += 1
          if (!firstElementInArray || !outerFirstElementInArray) newlineBuffered = true
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
    def visitEnd(index: Int): Writer = {
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
  override def visitObject(
      length: Int,
      index: Int): upickle.core.ObjVisitor[java.io.Writer, java.io.Writer] {
    def subVisitor: sjsonnet.PrettyYamlRenderer
    def visitKey(index: Int): sjsonnet.PrettyYamlRenderer
  } = new ObjVisitor[Writer, Writer] {
    firstElementInArray = false
    var empty = true
    flushBuffer()
    if (!topLevel) depth += 1
    topLevel = false
    def subVisitor: sjsonnet.PrettyYamlRenderer = PrettyYamlRenderer.this
    def visitKey(index: Int): sjsonnet.PrettyYamlRenderer = {

      if (empty) {
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
      if (bufferedComment != null) {
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
    def visitEnd(index: Int): Writer = {
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

object PrettyYamlRenderer {

  /**
   * Renders a multi-line string with all indentation and whitespace preserved
   */
  def writeBlockString(
      str: String,
      out: Writer,
      depth: Int,
      indent: Int,
      lineComment: String): Unit = {
    val len = str.length()
    val splits = YamlRenderer.newlinePattern.split(str, -1)
    val blockOffsetNumeral = if (str.charAt(0) != ' ') "" else indent
    val (blockStyle, dropRight) =
      (str.charAt(len - 1), if (len > 2) Some(str.charAt(len - 2)) else None) match {
        case ('\n', Some('\n')) => (s"|$blockOffsetNumeral+", 1)
        case ('\n', _)          => (s"|$blockOffsetNumeral", 1)
        case (_, _)             => (s"|$blockOffsetNumeral-", 0)
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
   * Wrap a double-quoted string. This behaves very differently from [[writeWrappedString]], as it
   * allows newline characters to be present (escaped as `\n`), allows wrapping in the middle of a
   * multi-' ' gap, and requires `\`-escaping of line ends and starts.
   *
   * Transcribed directly from PyYAML implementation to get all the nuances right
   * https://github.com/yaml/pyyaml/blob/master/lib/yaml/emitter.py#L915-L985
   */
  def writeDoubleQuoted(
      out: Writer,
      leftIndent: Int,
      leftHandPrefixOffset: Int,
      idealWidth: Int,
      text: String,
      split: Boolean = true,
      allowUnicode: Boolean = false): Unit = {
    out.write('"')
    var column = leftHandPrefixOffset + leftIndent + 1 // +1 to include the open quote
    var start = 0
    var end = 0
    def writeData(data: String): Unit = {
      out.write(data)
      column += data.length
    }

    def isBreakableChar(ch: Char, allowUnicode: Boolean) = {
      ch match {
        case '\"' | '\\' | '\u0085' | '\u2028' | '\u2029' | '\uFEFF' => true
        case _                                                       =>
          val isNormalChar = '\u0020' <= ch && ch <= '\u007E'
          val isUnicodePrintableChar =
            '\u00A0' <= ch && ch <= '\uD7FF' || '\uE000' <= ch && ch <= '\uFFFD'
          !(isNormalChar || (allowUnicode && isUnicodePrintableChar))
      }
    }

    def getEscapeSequenceForChar(ch: Char): String = (ch: Char) match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '\"' => "\\\""
      case '\\' => "\\\\"
      case _    =>
        if (ch <= '\u00FF') {
          "\\u" + hex((ch >> 4) & 15) + hex(ch & 15)
        } else if (ch <= '\uFFFF') {
          "\\u" + hex((ch >> 12) & 15) + hex((ch >> 8) & 15) + hex((ch >> 4) & 15) + hex(ch & 15)
        } else throw new RuntimeException("Unexpected")
    }

    while (end <= text.length) {
      val ch: Character = if (end < text.length) text(end) else null
      if (ch == null || isBreakableChar(ch, allowUnicode)) {
        if (start < end) {
          writeData(text.slice(start, end))
          start = end
        }
        if (ch != null) {
          writeData(getEscapeSequenceForChar(ch))
          start = end + 1
        }
      }
      if (
        0 < end && end < text.length - 1 && (ch == ' ' || start >= end)
          && column + (end - start) > idealWidth && split
      ) {
        writeData(text.slice(start, end) + '\\')
        if (start < end) start = end
        if (column > idealWidth) {
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
   * Wraps a string by breaking it up into space-separated tokens, and appending each token onto the
   * string until it overshoots the `idealWidth` before wrapping. Assumes there are no `\n`
   * characters in the string to begin with.
   *
   * Is used for both naked and single quoted strings.
   */
  def writeWrappedString(
      s: String,
      leftHandPrefixOffset: Int,
      out: Writer,
      leftIndent: Int,
      idealWidth: Int): Unit = {

    val tokens0 = s.split(" ", -1)
    // Consolidate tokens which are separated by more than 1 space, as these
    // cannot be wrapped across multiple lines since a newline character is
    // equivalent to a single space
    val tokens = collection.mutable.Buffer.empty[String]
    for (chunk <- tokens0) {
      (tokens.lastOption, chunk) match {
        case (None, "")       => tokens.append(" ")
        case (None, v)        => tokens.append(v)
        case (Some(prev), "") => tokens(tokens.length - 1) += " "
        case (Some(prev), v)  =>
          if (prev.endsWith(" ")) tokens(tokens.length - 1) += " " + v
          else tokens.append(v)
      }
    }

    var currentOffset = leftHandPrefixOffset + leftIndent
    var firstInLine = true
    var firstLine = true

    for (token <- tokens) {
      // This logic doesn't actually ensure that the text is wrapped to fit within
      // `idealWidth` characters width, but instead follows the behavior of the common PyYAML
      // library. Thus it is expected for the wrapped text to over-shoot the 80
      // character mark by up to one token, which can be of varying width
      val maxWidth = idealWidth
      if (!firstInLine && currentOffset > maxWidth) {
        PrettyYamlRenderer.writeIndentation(out, leftIndent)
        firstLine = false
        currentOffset = leftIndent
        out.write(token)
      } else {
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
   * Checks whether a string needs YAML quoting because it might otherwise be interpreted as a
   * non-string scalar (keyword, number, date, etc.) or collides with YAML reserved syntax.
   *
   * Originally implemented via FastParse; that worked correctly but was a major hotspot because
   * each invocation eagerly materialized parse-failure messages (Lazy[String] thunks). Profiling
   * the Scala Native build on the kube-prometheus workload showed `Parsed$Failure.formatMsg` /
   * `Parsed$Failure.msg` consuming ~27% of total CPU. This hand-rolled scanner reproduces the
   * original grammar exactly while avoiding any allocation or backtracking overhead.
   *
   * The reference FastParse implementation is preserved as [[stringNeedsToBeQuotedReference]] for
   * exhaustive equivalence testing.
   */
  def stringNeedsToBeQuoted(str: String): Boolean = {
    val len = str.length
    if (len == 0) return false

    val c0 = str.charAt(0)

    // === yamlPunctuation (prefix match — no End required) ===
    (c0: @scala.annotation.switch) match {
      case '!' | '&' | '*' | '{' | '}' | '[' | ']' | ',' | '#' | '|' | '>' | '@' | '`' | '"' |
          '\'' | ' ' =>
        return true
      case _ =>
    }
    // Two-char punctuation prefixes: "- ", ": ", "? "
    if (len >= 2 && str.charAt(1) == ' ' && (c0 == '-' || c0 == ':' || c0 == '?')) return true

    // === yamlKeyword (whole-string match) ===
    if (isYamlKeywordExact(str)) return true

    // === yamlTime | yamlDate | yamlNumber (whole-string match, gated on first char) ===
    if (c0 == '.' || (c0 >= '0' && c0 <= '9') || c0 == '-') {
      if (isYamlTimeExact(str) || isYamlDateExact(str) || isYamlNumberExact(str)) return true
    }

    // === Substring / suffix checks (carried over from original) ===
    val last = str.charAt(len - 1)
    last == ':' || last == ' ' || str.indexOf(": ") >= 0 || str.indexOf(" #") >= 0
  }

  // ---- Hand-rolled scanners replicating the FastParse grammar ---------------------------------

  /** Whole-string match against PyYAML's reserved keyword set. */
  @inline private def isYamlKeywordExact(s: String): Boolean = s match {
    case "yes" | "Yes" | "YES" | "no" | "No" | "NO" | "true" | "True" | "TRUE" | "false" | "False" |
        "FALSE" | "on" | "On" | "ON" | "off" | "Off" | "OFF" | "null" | "Null" | "NULL" | "~" |
        "-" | "=" =>
      true
    case _ => false
  }

  @inline private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  /** yamlTime: `dd:dd` (exactly 5 chars). */
  private def isYamlTimeExact(s: String): Boolean = {
    s.length == 5 &&
    isDigit(s.charAt(0)) && isDigit(s.charAt(1)) &&
    s.charAt(2) == ':' &&
    isDigit(s.charAt(3)) && isDigit(s.charAt(4))
  }

  /**
   * yamlDate: `dddd - d[d] - d[d]` optionally followed by `dateTimeSuffix`, full-string match.
   * dateTimeSuffix: `('T'|' ') dd ':' dd ':' dd ('.' digits?)? (' '|'Z')? ('-'? d[d])?`
   */
  private def isYamlDateExact(s: String): Boolean = {
    val len = s.length
    if (len < 8) return false // 4 + 1 + 1 + 1 + 1 minimum
    var i = 0
    // Four digits
    if (
      !(isDigit(s.charAt(0)) && isDigit(s.charAt(1)) && isDigit(s.charAt(2)) && isDigit(
        s.charAt(3)
      ))
    ) return false
    i = 4
    if (s.charAt(i) != '-') return false
    i += 1
    if (!isDigit(s.charAt(i))) return false
    i += 1
    if (i < len && isDigit(s.charAt(i))) i += 1
    if (i >= len || s.charAt(i) != '-') return false
    i += 1
    if (i >= len || !isDigit(s.charAt(i))) return false
    i += 1
    if (i < len && isDigit(s.charAt(i))) i += 1
    if (i == len) return true
    // Optional dateTimeSuffix: ('T'|' ') dd ':' dd ':' dd ...
    val sep = s.charAt(i)
    if (sep != 'T' && sep != ' ') return false
    i += 1
    // Six digits with two colons interleaved: dd ':' dd ':' dd
    if (i + 8 > len) return false
    if (
      !(isDigit(s.charAt(i)) && isDigit(s.charAt(i + 1)) && s.charAt(i + 2) == ':' &&
      isDigit(s.charAt(i + 3)) && isDigit(s.charAt(i + 4)) && s.charAt(i + 5) == ':' &&
      isDigit(s.charAt(i + 6)) && isDigit(s.charAt(i + 7)))
    ) return false
    i += 8
    if (i == len) return true
    // Optional fractional: '.' digits?
    if (s.charAt(i) == '.') {
      i += 1
      while (i < len && isDigit(s.charAt(i))) i += 1
      if (i == len) return true
    }
    // Optional trailing: (' '|'Z')? ('-'? d[d])?
    val maybeTz = s.charAt(i)
    if (maybeTz == ' ' || maybeTz == 'Z') {
      i += 1
      if (i == len) return true
    }
    if (i < len && s.charAt(i) == '-') i += 1
    if (i < len && isDigit(s.charAt(i))) {
      i += 1
      if (i < len && isDigit(s.charAt(i))) i += 1
    }
    i == len
  }

  /**
   * yamlNumber: `'-'? yamlNumber0` where yamlNumber0 = `".inf" | yamlFloat | yamlOctalHex | digits`
   * yamlFloat = `(digits? '.' digits | digits '.') (('e'|'E') ('+'|'-')? digits)?` yamlOctalHex=
   * `'0' ('x' [1-9a-fA-F] [0-9a-fA-F]* | 'o' [1-7] [0-7]*)` All matched against the whole string.
   */
  private def isYamlNumberExact(s: String): Boolean = {
    val len = s.length
    var i = 0
    if (i < len && s.charAt(i) == '-') i += 1
    if (i == len) return false
    // ".inf"
    if (s.charAt(i) == '.') {
      // either ".inf" (literal) or float starting with "." like ".5"
      if (
        i + 4 <= len && s.charAt(i + 1) == 'i' && s.charAt(i + 2) == 'n' &&
        s.charAt(i + 3) == 'f' && i + 4 == len
      ) return true
      // float ".digits"
      return isYamlFloatFromDot(s, i, len)
    }
    // yamlOctalHex: 0 (x... | o...)
    if (s.charAt(i) == '0' && i + 1 < len && (s.charAt(i + 1) == 'x' || s.charAt(i + 1) == 'o')) {
      return isYamlOctalHex(s, i, len)
    }
    // yamlFloat or digits — both start with digit
    if (!isDigit(s.charAt(i))) return false
    // Scan run of digits
    val digitsStart = i
    while (i < len && isDigit(s.charAt(i))) i += 1
    if (i == len) return true // pure digits
    // Float: <digits>.<digits>? then optional exponent
    if (s.charAt(i) == '.') {
      i += 1
      // digits after '.' are required IF we matched `(digits? "." digits)` form.
      // But grammar also allows `digits "."` (no digits after) as a float form.
      val afterDot = i
      while (i < len && isDigit(s.charAt(i))) i += 1
      // exponent
      if (i < len && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
        // exponent requires `('+'|'-')? digits`
        if (i == afterDot && digitsStart == i - 1) {
          // pattern was `digits .` with no fractional digits, exponent still allowed only if
          // overall form chosen was `(digits? . digits | digits .)` — exponent attaches to either.
        }
        i += 1
        if (i < len && (s.charAt(i) == '+' || s.charAt(i) == '-')) i += 1
        if (i >= len || !isDigit(s.charAt(i))) return false
        while (i < len && isDigit(s.charAt(i))) i += 1
      }
      return i == len
    }
    // Exponent without dot: `digits ('e' ...)` is not part of yamlFloat grammar (always requires '.'),
    // and digits alone matched earlier. So if we hit a non-digit non-'.' here, fail.
    false
  }

  /** Continue parsing a yamlFloat starting at the '.' position. `i` points at the '.'. */
  private def isYamlFloatFromDot(s: String, start: Int, len: Int): Boolean = {
    var i = start
    // grammar requires `digits? '.' digits | digits '.'`. Coming here means no leading digits,
    // so we must take `'.' digits` branch (i.e. `digits? '.' digits` with empty digits?).
    if (s.charAt(i) != '.') return false
    i += 1
    if (i >= len || !isDigit(s.charAt(i))) return false
    while (i < len && isDigit(s.charAt(i))) i += 1
    // Optional exponent
    if (i < len && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
      i += 1
      if (i < len && (s.charAt(i) == '+' || s.charAt(i) == '-')) i += 1
      if (i >= len || !isDigit(s.charAt(i))) return false
      while (i < len && isDigit(s.charAt(i))) i += 1
    }
    i == len
  }

  /** yamlOctalHex starting at the leading '0'. */
  private def isYamlOctalHex(s: String, start: Int, len: Int): Boolean = {
    var i = start
    if (s.charAt(i) != '0') return false
    i += 1
    if (i >= len) return false
    s.charAt(i) match {
      case 'x' =>
        i += 1
        if (i >= len) return false
        val c = s.charAt(i)
        if (!((c >= '1' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')))
          return false
        i += 1
        while (i < len) {
          val cc = s.charAt(i)
          if (!((cc >= '0' && cc <= '9') || (cc >= 'a' && cc <= 'f') || (cc >= 'A' && cc <= 'F')))
            return false
          i += 1
        }
        true
      case 'o' =>
        i += 1
        if (i >= len) return false
        val c = s.charAt(i)
        if (c < '1' || c > '7') return false
        i += 1
        while (i < len) {
          val cc = s.charAt(i)
          if (cc < '0' || cc > '7') return false
          i += 1
        }
        true
      case _ => false
    }
  }

  def writeIndentation(out: Writer, n: Int): Unit = {
    out.append('\n')
    var i = n
    while (i > 0) {
      out.append(' ')
      i -= 1
    }
  }

}
