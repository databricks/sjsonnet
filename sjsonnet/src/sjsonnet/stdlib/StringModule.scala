package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable

/**
 * Native implementations for Jsonnet standard-library entries in this module.
 *
 * Official Jsonnet stdlib documentation links for this module:
 *
 *   - [[https://jsonnet.org/ref/stdlib.html#std-toString std.toString(a)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-length std.length(x)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-codepoint std.codepoint(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-substr std.substr(str, from, len)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-startsWith std.startsWith(a, b)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-endsWith std.endsWith(a, b)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-char std.char(n)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-strReplace std.strReplace(str, from, to)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-stripChars std.stripChars(str, chars)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-lstripChars std.lstripChars(str, chars)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-rstripChars std.rstripChars(str, chars)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-join std.join(sep, arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-split std.split(str, c)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-splitLimit std.splitLimit(str, c, maxsplits)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-splitLimitR std.splitLimitR(str, c, maxsplits)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-stringChars std.stringChars(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-parseInt std.parseInt(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-parseOctal std.parseOctal(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-parseHex std.parseHex(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-asciiUpper std.asciiUpper(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-asciiLower std.asciiLower(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-encodeUTF8 std.encodeUTF8(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-decodeUTF8 std.decodeUTF8(arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-format std.format(str, vals)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-findSubstr std.findSubstr(pat, str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-isEmpty std.isEmpty(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-trim std.trim(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-equalsIgnoreCase std.equalsIgnoreCase(str1, str2)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-escapeStringJson std.escapeStringJson(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-escapeStringPython std.escapeStringPython(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-escapeStringXML std.escapeStringXML(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-escapeStringBash std.escapeStringBash(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-escapeStringDollars std.escapeStringDollars(str)]]
 */
object StringModule extends AbstractFunctionModule {
  def name = "string"

  private val whiteSpaces = StripUtils.codePointsSet(" \t\n\f\r\u0085\u00A0")

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-toString std.toString(a)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Convert the given argument to a string.
   */
  private object ToString extends Val.Builtin1("toString", "a") {
    def evalRhs(v1: Eval, ev: EvalScope, pos: Position): Val = Val.Str(
      pos,
      v1.value match {
        case Val.Str(_, s) => s
        case v             => Materializer.stringify(v)(ev)
      }
    )
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-length std.length(x)]].
   *
   * Since: 0.10.0. Group: Types and Reflection.
   *
   * Depending on the type of the value given, either returns the number of elements in the array,
   * the number of codepoints in the string, the number of parameters in the function, or the number
   * of fields in the object. Raises an error if given a primitive value, i.e. null, true or false.
   */
  private object Length extends Val.Builtin1("length", "x") {
    def evalRhs(x: Eval, ev: EvalScope, pos: Position): Val =
      Val.cachedNum(
        pos,
        (x.value match {
          case Val.Str(_, s) => s.codePointCount(0, s.length)
          case a: Val.Arr    => a.length
          case o: Val.Obj    => o.visibleKeyNames.length
          case o: Val.Func   => o.params.names.length
          case x             => Error.fail("Cannot get length of " + x.prettyName)
        }).toDouble
      )
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-codepoint std.codepoint(str)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns the positive integer representing the unicode codepoint of the character in the given
   * single-character string. This function is the inverse of std.char(n).
   */
  private object Codepoint extends Val.Builtin1("codepoint", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
      val s = str.value.asString
      val codePointCount = s.codePointCount(0, s.length)
      if (codePointCount != 1) {
        Error.fail("expected a single character string, got " + s)
      } else {
        Val.cachedNum(pos, s.codePointAt(0).toDouble)
      }
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-substr std.substr(str, from, len)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns a string that is the part of str that starts at offset from and is len codepoints long.
   * If the string str is shorter than from+len, the suffix starting at position from will be
   * returned.
   *
   * The slice operator (e.g., str[from:to]) can also be used on strings, as an alternative to this
   * function. However, note that the slice operator takes a start and an end index, but std.substr
   * takes a start index and a length.
   */
  private object Substr extends Val.Builtin3("substr", "str", "from", "len") {
    def evalRhs(_s: Eval, from: Eval, len: Eval, ev: EvalScope, pos: Position): Val = {
      val str = _s.value.asString
      val offset = from.value match {
        case v: Val.Num => v.asPositiveInt
        case _ => Error.fail("Expected a number for offset in substr, got " + from.value.prettyName)
      }
      val length = len.value match {
        case v: Val.Num => v.asPositiveInt
        case _ => Error.fail("Expected a number for len in substr, got " + len.value.prettyName)
      }

      if (length <= 0) {
        Val.Str(pos, "")
      } else {
        val requestedEnd = offset.toLong + length.toLong
        if (
          requestedEnd <= str.length &&
          StringUtils.utf16OffsetsAreCodePointOffsets(str, requestedEnd.toInt)
        ) {
          // Before the requested end, every UTF-16 offset is also a codepoint offset. This keeps
          // the common ASCII/BMP path on substring without scanning the rest of a long string.
          Val.Str(pos, str.substring(offset, requestedEnd.toInt))
        } else {
          val unicodeLength = str.codePointCount(0, str.length)
          val safeOffset = math.min(offset, unicodeLength)
          val safeLength = math.min(length, unicodeLength - safeOffset)

          if (safeLength <= 0) {
            Val.Str(pos, "")
          } else {
            val startUtf16 = if (safeOffset == 0) 0 else str.offsetByCodePoints(0, safeOffset)
            val endUtf16 = str.offsetByCodePoints(startUtf16, safeLength)
            Val.Str(pos, str.substring(startUtf16, endUtf16))
          }
        }
      }
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-startsWith std.startsWith(a, b)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns whether the string a is prefixed by the string b.
   */
  private object StartsWith extends Val.Builtin2("startsWith", "a", "b") {
    def evalRhs(a: Eval, b: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(a.value.asString.startsWith(b.value.asString))
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-endsWith std.endsWith(a, b)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns whether the string a is suffixed by the string b.
   */
  private object EndsWith extends Val.Builtin2("endsWith", "a", "b") {
    def evalRhs(a: Eval, b: Eval, ev: EvalScope, pos: Position): Val =
      Val.bool(a.value.asString.endsWith(b.value.asString))
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-char std.char(n)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns a string of length one whose only unicode codepoint has integer id n. This function is
   * the inverse of std.codepoint(str).
   */
  private object Char_ extends Val.Builtin1("char", "n") {
    def evalRhs(n: Eval, ev: EvalScope, pos: Position): Val = {
      val c = n.value.asInt
      if (!Character.isValidCodePoint(c)) {
        Error.fail(s"Invalid unicode code point, got " + c)
      }
      Val.Str(pos, Character.toString(c))
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-strReplace std.strReplace(str, from, to)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns a copy of the string in which all occurrences of string from have been replaced with
   * string to.
   */
  private object StrReplace extends Val.Builtin3("strReplace", "str", "from", "to") {
    def evalRhs(str: Eval, from: Eval, to: Eval, ev: EvalScope, pos: Position): Val = {
      val fromForce = from.value.asString
      if (fromForce.isEmpty) {
        Error.fail("Cannot replace empty string in strReplace")
      }
      Val.Str(pos, str.value.asString.replace(fromForce, to.value.asString))
    }
  }

  private object StringUtils {
    @inline def utf16OffsetsAreCodePointOffsets(str: String): Boolean = {
      utf16OffsetsAreCodePointOffsets(str, str.length)
    }

    @inline def utf16OffsetsAreCodePointOffsets(str: String, endExclusive: Int): Boolean = {
      var i = 0
      while (i < endExclusive) {
        if (Character.isHighSurrogate(str.charAt(i))) return false
        i += 1
      }
      true
    }
  }

  private object StripUtils {
    def codePointsSet(str: String): collection.Set[Int] = {
      val chars = Set.newBuilder[Int]
      chars.sizeHint(str.codePointCount(0, str.length))
      var i = 0
      while (i < str.length) {
        val codePoint = str.codePointAt(i)
        chars += codePoint
        i += Character.charCount(codePoint)
      }
      chars.result()
    }

    /**
     * Optimized strip implementation for std.stripChars/lstripChars/rstripChars. Most strip sets
     * are ASCII/BMP delimiters; handling them before building a boxed Set[Int] keeps the hot path
     * allocation-light and lets the JVM inline the membership checks.
     */
    def strip(str: String, chars: String, left: Boolean, right: Boolean): String = {
      if (str.isEmpty || chars.isEmpty) return str

      val single = singleBmpNonSurrogate(chars)
      if (single >= 0) {
        return stripSingleChar(str, single.toChar, left, right)
      }

      val bmpSet = bmpNonSurrogateSet(chars)
      if (bmpSet != null) {
        return stripBmp(str, bmpSet, left, right)
      }

      unspecializedStrip(str, codePointsSet(chars), left, right)
    }

    def unspecializedStrip(
        str: String,
        charsSet: collection.Set[Int],
        left: Boolean,
        right: Boolean): String = {
      if (str.isEmpty) return str

      // General case: full codepoint-based iteration (handles surrogate pairs)
      var start = 0
      var end = str.length
      while (left && start < end && charsSet.contains(str.codePointAt(start))) {
        start = str.offsetByCodePoints(start, 1)
      }
      while (right && end > start && charsSet.contains(str.codePointBefore(end))) {
        end = str.offsetByCodePoints(end, -1)
      }
      str.substring(start, end)
    }

    private def singleBmpNonSurrogate(chars: String): Int =
      if (chars.length == 1 && !Character.isSurrogate(chars.charAt(0))) chars.charAt(0).toInt
      else -1

    private def bmpNonSurrogateSet(chars: String): java.util.BitSet = {
      val set = new java.util.BitSet(128)
      var i = 0
      while (i < chars.length) {
        val ch = chars.charAt(i)
        if (Character.isSurrogate(ch)) return null
        set.set(ch.toInt)
        i += 1
      }
      set
    }

    /**
     * Fast path for stripping a single non-surrogate BMP character. Surrogate pairs in `str` are
     * safe here: neither half can equal the non-surrogate delimiter.
     */
    private def stripSingleChar(str: String, ch: Char, left: Boolean, right: Boolean): String = {
      var start = 0
      var end = str.length
      if (left) {
        while (start < end && str.charAt(start) == ch) start += 1
      }
      if (right) {
        while (end > start && str.charAt(end - 1) == ch) end -= 1
      }
      str.substring(start, end)
    }

    /**
     * Medium path for non-surrogate BMP delimiter sets. BitSet avoids boxed Set[Int] lookups; valid
     * surrogate pairs in `str` are not stripped because high/low surrogate chars are never in set.
     */
    private def stripBmp(
        str: String,
        charsSet: java.util.BitSet,
        left: Boolean,
        right: Boolean): String = {
      var start = 0
      var end = str.length
      if (left) {
        while (start < end && charsSet.get(str.charAt(start).toInt)) start += 1
      }
      if (right) {
        while (end > start && charsSet.get(str.charAt(end - 1).toInt)) end -= 1
      }
      str.substring(start, end)
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-stripChars std.stripChars(str, chars)]].
   *
   * Since: 0.15.0. Group: String Manipulation.
   *
   * Removes characters chars from the beginning and from the end of str.
   */
  private object StripChars extends Val.Builtin2("stripChars", "str", "chars") {
    def evalRhs(str: Eval, chars: Eval, ev: EvalScope, pos: Position): Val = {
      Val.Str(
        pos,
        StripUtils.strip(
          str.value.asString,
          chars.value.asString,
          left = true,
          right = true
        )
      )
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-lstripChars std.lstripChars(str, chars)]].
   *
   * Since: 0.15.0. Group: String Manipulation.
   *
   * Removes characters chars from the beginning of str.
   */
  private object LStripChars extends Val.Builtin2("lstripChars", "str", "chars") {
    def evalRhs(str: Eval, chars: Eval, ev: EvalScope, pos: Position): Val = {
      Val.Str(
        pos,
        StripUtils.strip(
          str.value.asString,
          chars.value.asString,
          left = true,
          right = false
        )
      )
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-rstripChars std.rstripChars(str, chars)]].
   *
   * Since: 0.15.0. Group: String Manipulation.
   *
   * Removes characters chars from the end of str.
   */
  private object RStripChars extends Val.Builtin2("rstripChars", "str", "chars") {
    def evalRhs(str: Eval, chars: Eval, ev: EvalScope, pos: Position): Val = {
      Val.Str(
        pos,
        StripUtils.strip(
          str.value.asString,
          chars.value.asString,
          left = false,
          right = true
        )
      )
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-join std.join(sep, arr)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * If sep is a string, then arr must be an array of strings, in which case they are concatenated
   * with sep used as a delimiter. If sep is an array, then arr must be an array of arrays, in which
   * case the arrays are concatenated in the same way, to produce a single array.
   */
  private object Join extends Val.Builtin2("join", "sep", "arr") {
    private def appendArray(out: mutable.ArrayBuilder.ofRef[Eval], arr: Val.Arr): Unit = {
      arr.copyEvalTo(out)
    }

    def evalRhs(sep: Eval, _arr: Eval, ev: EvalScope, pos: Position): Val = {
      val arr = implicitly[ReadWriter[Val.Arr]].apply(_arr.value)
      sep.value match {
        case Val.Str(_, s) =>
          val b = new java.lang.StringBuilder()
          var i = 0
          var added = false
          while (i < arr.length) {
            arr.value(i) match {
              case _: Val.Null   =>
              case Val.Str(_, x) =>
                if (added) b.append(s)
                added = true
                b.append(x)
              case x => Error.fail("Cannot join " + x.prettyName)
            }
            i += 1
          }
          Val.Str(pos, b.toString)
        case sep: Val.Arr =>
          val out = new mutable.ArrayBuilder.ofRef[Eval]
          // Set a reasonable size hint based on estimated result size
          out.sizeHint(arr.length * 2)
          var added = false
          for (x <- arr) {
            x match {
              case Val.Null(_) => // do nothing
              case v: Val.Arr  =>
                if (added) appendArray(out, sep)
                added = true
                appendArray(out, v)
              case x => Error.fail("Cannot join " + x.prettyName)
            }
          }
          Val.Arr(pos, out.result())
        case x => Error.fail("Cannot join " + x.prettyName)
      }
    }
  }

  private def splitLimit(pos: Position, str: String, cStr: String, maxSplits: Int): Array[Eval] = {
    if (cStr.isEmpty) {
      Error.fail("Cannot split by an empty string")
    }

    val b = new mutable.ArrayBuilder.ofRef[Eval]
    if (maxSplits >= 0 && maxSplits < Int.MaxValue) b.sizeHint(maxSplits + 1)
    var sz = 0
    var start = 0
    var next = if (maxSplits == 0) -1 else str.indexOf(cStr, start)

    while (next >= 0 && (maxSplits < 0 || sz < maxSplits)) {
      b += Val.Str(pos, str.substring(start, next))
      start = next + cStr.length
      sz += 1
      next = if (maxSplits >= 0 && sz >= maxSplits) -1 else str.indexOf(cStr, start)
    }
    b += Val.Str(pos, str.substring(start))
    sz += 1
    b.result()
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-split std.split(str, c)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Split the string str into an array of strings, divided by the string c.
   *
   * Note: Versions up to and including 0.18.0 require c to be a single character.
   */
  private object Split extends Val.Builtin2("split", "str", "c") {
    def evalRhs(str: Eval, c: Eval, ev: EvalScope, pos: Position): Val = {
      Val.Arr(pos, splitLimit(pos, str.value.asString, c.value.asString, -1))
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-splitLimit std.splitLimit(str, c, maxsplits)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * As std.split(str, c) but will stop after maxsplits splits, thereby the largest array it will
   * return has length maxsplits + 1. A limit of -1 means unlimited.
   *
   * Note: Versions up to and including 0.18.0 require c to be a single character.
   */
  private object SplitLimit extends Val.Builtin3("splitLimit", "str", "c", "maxsplits") {
    def evalRhs(str: Eval, c: Eval, maxSplits: Eval, ev: EvalScope, pos: Position): Val = {
      Val.Arr(pos, splitLimit(pos, str.value.asString, c.value.asString, maxSplits.value.asInt))
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-splitLimitR std.splitLimitR(str, c, maxsplits)]].
   *
   * Since: 0.19.0. Group: String Manipulation.
   *
   * As std.splitLimit(str, c, maxsplits) but will split from right to left.
   */
  private object SplitLimitR extends Val.Builtin3("splitLimitR", "str", "c", "maxsplits") {
    def evalRhs(str: Eval, c: Eval, maxSplits: Eval, ev: EvalScope, pos: Position): Val = {
      Val.Arr(
        pos,
        splitLimit(pos, str.value.asString.reverse, c.value.asString.reverse, maxSplits.value.asInt)
          .map(s => Val.Str(pos, s.value.asString.reverse))
          .reverse
      )
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-stringChars std.stringChars(str)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Split the string str into an array of strings, each containing a single codepoint.
   */
  private object StringChars extends Val.Builtin1("stringChars", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val =
      stringChars(pos, str.value.asString)
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-parseInt std.parseInt(str)]].
   *
   * Since: 0.10.0. Group: Parsing.
   *
   * Parses a signed decimal integer from the input string.
   */
  private object ParseInt extends Val.Builtin1("parseInt", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
      val s = str.value.asString
      if (s.isEmpty || s == "-") {
        Error.fail("Cannot parse '" + s + "' as an integer in base 10")
      }
      if (s.charAt(0) == '-') Val.cachedNum(pos, -parseNat(s, 1, 10, "base 10"))
      else Val.cachedNum(pos, parseNat(s, 0, 10, "base 10"))
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-parseOctal std.parseOctal(str)]].
   *
   * Since: 0.10.0. Group: Parsing.
   *
   * Parses an unsigned octal integer from the input string. Initial zeroes are tolerated.
   */
  private object ParseOctal extends Val.Builtin1("parseOctal", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
      val s = str.value.asString
      if (s.isEmpty) Error.fail("Cannot parse '' as an integer in base 8")
      Val.cachedNum(pos, parseNat(s, 0, 8, "base 8"))
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-parseHex std.parseHex(str)]].
   *
   * Since: 0.10.0. Group: Parsing.
   *
   * Parses an unsigned hexadecimal integer, from the input string. Case insensitive.
   */
  private object ParseHex extends Val.Builtin1("parseHex", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
      val s = str.value.asString
      if (s.isEmpty) Error.fail("Cannot parse '' as an integer in base 16")
      Val.cachedNum(pos, parseNat(s, 0, 16, "base 16"))
    }
  }

  private def parseNat(str: String, start: Int, base: Int, baseName: String): Double = {
    if (base == 16) parseHexNat(str, start, baseName)
    else parseDigitNat(str, start, base, baseName)
  }

  private def parseDigitNat(str: String, start: Int, base: Int, baseName: String): Double = {
    var acc = 0.0
    var i = start
    while (i < str.length) {
      // Decimal and octal only accept ASCII digits; charAt keeps the hot path branch-light and
      // avoids the codePointAt/charCount work that always leads to an error for non-ASCII input.
      val digit = str.charAt(i) - '0'
      if (digit < 0 || digit >= base) {
        Error.fail("Cannot parse '" + str + "' as an integer in " + baseName)
      }
      acc = base * acc + digit
      i += 1
    }
    acc
  }

  private def parseHexNat(str: String, start: Int, baseName: String): Double = {
    var acc = 0.0
    var i = start
    while (i < str.length) {
      val code = str.charAt(i)
      // Keep the official digit mapping used by go-jsonnet/C++ jsonnet, where ':'..'?' map to
      // 10..15 in addition to A-F/a-f.
      val digit =
        if (code >= 'a') code - 'a' + 10
        else if (code >= 'A') code - 'A' + 10
        else code - '0'
      if (digit < 0 || digit >= 16) {
        Error.fail("Cannot parse '" + str + "' as an integer in " + baseName)
      }
      acc = 16.0 * acc + digit
      i += 1
    }
    acc
  }

  // Official asciiUpper/asciiLower convert ASCII letters only; non-ASCII codepoints are preserved.
  private def asciiUpper(str: String): String = {
    var out: Array[Char] = null
    var i = 0
    while (i < str.length) {
      val c = str.charAt(i)
      if (c >= 'a' && c <= 'z') {
        if (out == null) out = str.toCharArray
        out(i) = (c - ('a' - 'A')).toChar
      }
      i += 1
    }
    if (out == null) str else new String(out)
  }

  private def asciiLower(str: String): String = {
    var out: Array[Char] = null
    var i = 0
    while (i < str.length) {
      val c = str.charAt(i)
      if (c >= 'A' && c <= 'Z') {
        if (out == null) out = str.toCharArray
        out(i) = (c + ('a' - 'A')).toChar
      }
      i += 1
    }
    if (out == null) str else new String(out)
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-asciiUpper std.asciiUpper(str)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns a copy of the string in which all ASCII letters are capitalized.
   */
  private object AsciiUpper extends Val.Builtin1("asciiUpper", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, asciiUpper(str.value.asString))
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-asciiLower std.asciiLower(str)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns a copy of the string in which all ASCII letters are lower cased.
   */
  private object AsciiLower extends Val.Builtin1("asciiLower", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, asciiLower(str.value.asString))
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-encodeUTF8 std.encodeUTF8(str)]].
   *
   * Since: 0.13.0. Group: Parsing.
   *
   * Encode a string using UTF8. Returns an array of numbers representing bytes.
   */
  private object EncodeUTF8 extends Val.Builtin1("encodeUTF8", "str") {
    def evalRhs(s: Eval, ev: EvalScope, pos: Position): Val = {
      val bytes = s.value.asString.getBytes(UTF_8)
      val arr = new Array[Eval](bytes.length)
      var i = 0
      while (i < bytes.length) {
        arr(i) = Val.cachedNum(pos, (bytes(i) & 0xff).toDouble)
        i += 1
      }
      Val.Arr(pos, arr)
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-decodeUTF8 std.decodeUTF8(arr)]].
   *
   * Since: 0.13.0. Group: Parsing.
   *
   * Decode an array of numbers representing bytes using UTF8. Returns a string.
   */
  private object DecodeUTF8 extends Val.Builtin1("decodeUTF8", "arr") {
    def evalRhs(arr: Eval, ev: EvalScope, pos: Position): Val = {
      for ((v, idx) <- arr.value.asArr.iterator.zipWithIndex) {
        if (!v.isInstanceOf[Val.Num] || !v.asDouble.isWhole || v.asInt < 0 || v.asInt > 255) {
          throw Error.fail(
            f"Element $idx of the provided array was not an integer in range [0,255]"
          )
        }
      }
      Val.Str(
        pos,
        new String(arr.value.asArr.iterator.map(_.asInt.toByte).toArray, UTF_8)
      )
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-format std.format(str, vals)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Format the string str using the values in vals. The values can be an array, an object, or in
   * other cases are treated as if they were provided in a singleton array. The string formatting
   * follows the same rules as Python. The % operator can be used as a shorthand for this function.
   */
  private object Format_ extends Val.Builtin2("format", "str", "vals") {
    def evalRhs(str: Eval, vals: Eval, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Format.format(str.value.asString, vals.value, pos)(ev))
    override def specialize(args: Array[Expr], tailstrict: Boolean): (Val.Builtin, Array[Expr]) =
      args match {
        case Array(str, fmt: Val.Str) =>
          try { (new Format.PartialApplyFmt(fmt.str), Array(str)) }
          catch { case _: Exception => null }
        case _ => null
      }
  }

  private def stdToString(v: Val)(implicit ev: EvalScope): String =
    v match {
      case Val.Str(_, s) => s
      case other         => Materializer.stringify(other)(ev)
    }

  private def stringChars(pos: Position, str: String): Val.Arr = {
    val chars = new Array[Eval](str.codePointCount(0, str.length))
    var charIndex = 0
    var i = 0
    while (i < str.length) {
      val codePoint = str.codePointAt(i)
      chars(charIndex) = Val.Str(pos, Character.toString(codePoint))
      i += Character.charCount(codePoint)
      charIndex += 1
    }
    Val.Arr(pos, chars)
  }

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin(ToString),
    builtin(Length),
    builtin(Codepoint),
    builtin(Substr),
    builtin(StartsWith),
    builtin(EndsWith),
    builtin(Char_),
    builtin(StrReplace),
    builtin(StripChars),
    builtin(LStripChars),
    builtin(RStripChars),
    builtin(Join),
    builtin(Split),
    builtin(SplitLimit),
    builtin(SplitLimitR),
    builtin("resolvePath", "f", "r") { (_, _, f: String, r: String) =>
      val slash = f.lastIndexOf('/')
      if (slash < 0) r
      else f.substring(0, slash + 1) + r
    },
    builtin(StringChars),
    builtin(ParseInt),
    builtin(ParseOctal),
    builtin(ParseHex),
    builtin(AsciiUpper),
    builtin(AsciiLower),
    builtin(EncodeUTF8),
    builtin(DecodeUTF8),
    builtin(Format_),
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-findSubstr std.findSubstr(pat, str)]].
     *
     * Since: 0.10.0. Group: String Manipulation.
     *
     * Returns an array that contains the indexes of all occurrences of pat in str.
     */
    builtin("findSubstr", "pat", "str") { (pos, ev, pat: String, str: String) =>
      if (pat.isEmpty) Val.Arr(pos, new Array[Eval](0))
      else {
        var matchIndex = str.indexOf(pat)
        if (matchIndex == -1) Val.Arr(pos, new Array[Eval](0))
        else {
          val indices = new mutable.ArrayBuilder.ofRef[Val.Num]

          if (StringUtils.utf16OffsetsAreCodePointOffsets(str)) {
            // String.indexOf returns UTF-16 offsets. For BMP-only strings those are already
            // Jsonnet codepoint offsets, so avoid a codePointCount scan for every match.
            while (0 <= matchIndex && matchIndex < str.length) {
              indices.+=(Val.cachedNum(pos, matchIndex.toDouble))
              matchIndex = str.indexOf(pat, matchIndex + 1)
            }
          } else {
            // Compute codepoint indices incrementally, avoiding an O(n) calculation for each match.
            var prevCharIndex = 0
            var prevCodePointIndex = 0

            while (0 <= matchIndex && matchIndex < str.length) {
              val codePointIndex =
                prevCodePointIndex + str.codePointCount(prevCharIndex, matchIndex)
              indices.+=(Val.cachedNum(pos, codePointIndex.toDouble))

              prevCharIndex = matchIndex
              prevCodePointIndex = codePointIndex
              matchIndex = str.indexOf(pat, matchIndex + 1)
            }
          }
          Val.Arr(pos, indices.result())
        }
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-isEmpty std.isEmpty(str)]].
     *
     * Since: 0.20.0. Group: String Manipulation.
     *
     * Returns true if the given string is of zero length.
     */
    builtin("isEmpty", "str") { (_, _, value: Val) =>
      value match {
        case Val.Str(_, s) => s.isEmpty
        case a: Val.Arr    => a.length == 0
        case o: Val.Obj    => o.visibleKeyNames.isEmpty
        case f: Val.Func   => f.params.names.isEmpty
        case x             =>
          Error.fail("length operates on strings, objects, and arrays, got " + x.prettyName)
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-trim std.trim(str)]].
     *
     * Since: 0.21.0. Group: String Manipulation.
     *
     * Returns a copy of string after eliminating leading and trailing whitespaces.
     */
    builtin("trim", "str") { (_, _, str: String) =>
      StripUtils.unspecializedStrip(str, whiteSpaces, true, true)
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-equalsIgnoreCase std.equalsIgnoreCase(str1, str2)]].
     *
     * Since: 0.21.0. Group: String Manipulation.
     *
     * Returns true if the the given str1 is equal to str2 by doing case insensitive comparison,
     * false otherwise.
     */
    builtin("equalsIgnoreCase", "str1", "str2") { (_, _, str1: String, str2: String) =>
      // Official equalsIgnoreCase is defined through asciiLower, not Unicode case folding.
      asciiLower(str1) == asciiLower(str2)
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-escapeStringJson std.escapeStringJson(str)]].
     *
     * Since: 0.10.0. Group: String Manipulation.
     *
     * Convert str to allow it to be embedded in a JSON representation, within a string. This adds
     * quotes, escapes backslashes, and escapes unprintable characters.
     */
    builtin("escapeStringJson", "str_") { (pos, ev, str: Val) =>
      if (str.value.isInstanceOf[Val.Str]) {
        Materializer.stringify(str)(ev)
      } else {
        val out = new java.io.StringWriter()
        BaseRenderer.escape(out, Materializer.stringify(str)(ev), unicode = true)
        out.toString
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-escapeStringPython std.escapeStringPython(str)]].
     *
     * Since: 0.10.0. Group: String Manipulation.
     *
     * Convert str to allow it to be embedded in Python. This is an alias for std.escapeStringJson.
     */
    builtin("escapeStringPython", "str") { (pos, ev, str: Val) =>
      val out = new java.io.StringWriter()
      BaseRenderer.escape(out, stdToString(str)(ev), unicode = true)
      out.toString
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-escapeStringXML std.escapeStringXML(str)]].
     *
     * Since: 0.10.0. Group: String Manipulation.
     *
     * Convert str to allow it to be embedded in XML (or HTML). It escapes XML-sensitive characters
     * (<, >, &, ", and ').
     */
    builtin("escapeStringXML", "str") { (_, ev, str: Val) =>
      val string = stdToString(str)(ev)
      val out = new java.io.StringWriter()
      var i = 0
      while (i < string.length) {
        string.charAt(i) match {
          case '<'  => out.write("&lt;")
          case '>'  => out.write("&gt;")
          case '&'  => out.write("&amp;")
          case '"'  => out.write("&quot;")
          case '\'' => out.write("&apos;")
          case c    => out.write(c)
        }
        i += 1
      }
      out.toString
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-escapeStringBash std.escapeStringBash(str)]].
     *
     * Since: 0.10.0. Group: String Manipulation.
     *
     * Wrap str in single quotes, and escape any single quotes within str by changing them to a
     * sequence '"'"'. This allows injection of arbitrary strings as arguments of commands in bash
     * scripts.
     */
    builtin("escapeStringBash", "str_") { (pos, ev, str: Val) =>
      "'" + stdToString(str)(ev).replace("'", """'"'"'""") + "'"
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-escapeStringDollars std.escapeStringDollars(str)]].
     *
     * Since: 0.10.0. Group: String Manipulation.
     *
     * Convert $ to $$ in str. This allows injection of arbitrary strings into systems that use $
     * for string interpolation (like Terraform).
     */
    builtin("escapeStringDollars", "str_") { (pos, ev, str: Val) =>
      stdToString(str)(ev).replace("$", "$$")
    }
  )
}
