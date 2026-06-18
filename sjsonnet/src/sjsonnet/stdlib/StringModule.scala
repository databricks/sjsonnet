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
  private val PresizedCopyMaxParts = 1024

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
          case v: Val.Str =>
            val s = v.str
            if (v.isInstanceOf[Val.AsciiSafeStr]) s.length
            else s.codePointCount(0, s.length)
          case a: Val.Arr  => a.length
          case o: Val.Obj  => o.visibleKeyNames.length
          case o: Val.Func => o.params.names.length
          case x           => Error.fail("Cannot get length of " + x.prettyName)
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
      val srcVal = _s.value
      val str = srcVal.asString
      val srcAsciiSafe = srcVal.isInstanceOf[Val.AsciiSafeStr]
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
      } else if (srcAsciiSafe) {
        val strLen = str.length
        val safeOffset = math.min(offset, strLen)
        val safeLength = math.min(length, strLen - safeOffset)
        if (safeLength <= 0) Val.Str(pos, "")
        else
          Val.Str.asciiSafe(pos, str.substring(safeOffset, safeOffset + safeLength))
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
      val c0 = n.value.asInt
      if (!Character.isValidCodePoint(c0)) {
        Error.fail(s"Invalid unicode code point, got " + c0)
      }
      val c = if (c0 >= 0xd800 && c0 <= 0xdfff) 0xfffd else c0
      val s = Character.toString(c)
      // Single-codepoint result; ASCII printable except '"' and '\\' is JSON-safe.
      if (c >= 0x20 && c < 0x7f && c != '"' && c != '\\') Val.Str.asciiSafe(pos, s)
      else Val.Str(pos, s)
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
        Error.fail("'from' string must not be zero length.", pos)(ev)
      }
      val srcVal = str.value
      val toVal = to.value
      val out = srcVal.asString.replace(fromForce, toVal.asString)
      val srcSafe = srcVal.isInstanceOf[Val.AsciiSafeStr]
      val toSafe = toVal.isInstanceOf[Val.AsciiSafeStr]
      if (srcSafe && toSafe) Val.Str.asciiSafe(pos, out) else Val.Str(pos, out)
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

  private[sjsonnet] object StripUtils {
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

      // Common case: strip set fits in a byte (chars < 256). Build a 256-byte lookup table —
      // a single array load per char replaces the two conditional branches in the bitmask approach.
      var allByte = true
      var i = 0
      while (allByte && i < chars.length) {
        if (chars.charAt(i) >= 256) allByte = false
        i += 1
      }
      if (allByte) {
        val table = new Array[Byte](256)
        i = 0
        while (i < chars.length) {
          table(chars.charAt(i).toInt) = 1
          i += 1
        }
        return stripLookup(str, table, left, right)
      }

      val bmpSet = bmpNonSurrogateSet(chars)
      if (bmpSet != null) {
        return stripBmp(str, bmpSet, left, right)
      }

      unspecializedStrip(str, codePointsSet(chars), left, right)
    }

    /**
     * Fast path for strip sets where all chars fit in a byte (< 256). A single array load per char
     * replaces the two conditional branches in the bitmask approach.
     */
    private def stripLookup(
        str: String,
        table: Array[Byte],
        left: Boolean,
        right: Boolean): String = {
      var start = 0
      var end = str.length
      if (left) {
        var continue = true
        while (start < end && continue) {
          val c = str.charAt(start).toInt
          if (c >= 256 || table(c) == 0) {
            continue = false
          } else {
            start += 1
          }
        }
      }
      if (right) {
        var continue = true
        while (end > start && continue) {
          val c = str.charAt(end - 1).toInt
          if (c >= 256 || table(c) == 0) {
            continue = false
          } else {
            end -= 1
          }
        }
      }
      str.substring(start, end)
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
  private def charsToString(charsVal: Val): String = charsVal match {
    case s: Val.Str => s.str
    case a: Val.Arr =>
      val sb = new java.lang.StringBuilder
      var i = 0
      while (i < a.length) {
        a.value(i) match {
          case s: Val.Str if s.str.codePointCount(0, s.str.length) == 1 =>
            sb.append(s.str)
          case _ =>
        }
        i += 1
      }
      sb.toString
    case other => Error.fail(s"expected string or array, got ${other.prettyName}")
  }

  private object StripChars extends Val.Builtin2("stripChars", "str", "chars") {
    def evalRhs(str: Eval, chars: Eval, ev: EvalScope, pos: Position): Val = {
      val v = str.value
      val out = StripUtils.strip(
        v.asString,
        charsToString(chars.value),
        left = true,
        right = true
      )
      v match {
        case _: Val.AsciiSafeStr => Val.Str.asciiSafe(pos, out)
        case _                   => Val.Str(pos, out)
      }
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
      val v = str.value
      val out = StripUtils.strip(
        v.asString,
        charsToString(chars.value),
        left = true,
        right = false
      )
      v match {
        case _: Val.AsciiSafeStr => Val.Str.asciiSafe(pos, out)
        case _                   => Val.Str(pos, out)
      }
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
      val v = str.value
      val out = StripUtils.strip(
        v.asString,
        charsToString(chars.value),
        left = false,
        right = true
      )
      v match {
        case _: Val.AsciiSafeStr => Val.Str.asciiSafe(pos, out)
        case _                   => Val.Str(pos, out)
      }
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
    private def joinedRepeatedString(
        pos: Position,
        sep: Val.Str,
        str: Val.Str,
        count: Int): Val.Str = {
      if (count == 0) Val.Str.asciiSafe(pos, "")
      else {
        val s = str.str
        val sepStr = sep.str
        val resultLen = s.length.toLong * count + sepStr.length.toLong * (count - 1)
        if (resultLen > Int.MaxValue) Error.fail("String is too large to join")
        if (count == 1) str
        else {
          val asciiSafe = str.isInstanceOf[Val.AsciiSafeStr] && sep.isInstanceOf[Val.AsciiSafeStr]

          val b = new java.lang.StringBuilder(resultLen.toInt)
          if (s.length + sepStr.length <= 64) {
            val repeated = s + sepStr
            var i = 1
            while (i < count) {
              b.append(repeated)
              i += 1
            }
          } else {
            var i = 1
            while (i < count) {
              b.append(s)
              b.append(sepStr)
              i += 1
            }
          }
          b.append(s)

          val result = b.toString
          if (asciiSafe) Val.Str.asciiSafe(pos, result) else Val.Str(pos, result)
        }
      }
    }

    private def joinRepeatedStringEval(
        pos: Position,
        sep: Val.Str,
        elem: Eval,
        len: Int): Val.Str = {
      if (len == 0) return Val.Str.asciiSafe(pos, "")

      elem match {
        case _: Val.Null => Val.Str.asciiSafe(pos, "")
        case s: Val.Str  => joinedRepeatedString(pos, sep, s, len)
        case _: Val      => null
        case _           => null
      }
    }

    private def joinRepeatedDirectString(
        pos: Position,
        sep: Val.Str,
        direct: Array[Eval],
        len: Int): Val.Str = {
      val firstEval = direct(0)
      var i = 1
      while (i < len && (direct(i) eq firstEval)) i += 1
      if (i == len) joinRepeatedStringEval(pos, sep, firstEval, len)
      else null
    }

    private final val PresizedStringJoinMinParts = 16

    private def joinPresizedStringArray(
        pos: Position,
        sep: Val.Str,
        arr: Val.Arr,
        len: Int): Val.Str = {
      val sepStr = sep.str
      val sepLen = sepStr.length
      var totalLen = 0L
      var added = false
      var asciiSafe = true
      var i = 0
      while (i < len) {
        arr.value(i) match {
          case _: Val.Null =>
          case x: Val.Str  =>
            if (added) {
              totalLen += sepLen
              asciiSafe &&= sep.isInstanceOf[Val.AsciiSafeStr]
            }
            val str = x.str
            totalLen += str.length
            if (totalLen > Int.MaxValue) Error.fail("String is too large to join")
            asciiSafe &&= x.isInstanceOf[Val.AsciiSafeStr]
            added = true
          case x => Error.fail("Cannot join " + x.prettyName)
        }
        i += 1
      }

      if (!added) return Val.Str.asciiSafe(pos, "")

      val b = new java.lang.StringBuilder(totalLen.toInt)
      i = 0
      var needsSep = false
      while (i < len) {
        arr.value(i) match {
          case _: Val.Null =>
          case x: Val.Str  =>
            if (needsSep) b.append(sepStr)
            needsSep = true
            b.append(x.str)
          case _ =>
        }
        i += 1
      }
      val result = b.toString
      if (asciiSafe) Val.Str.asciiSafe(pos, result) else Val.Str(pos, result)
    }

    private def joinDirectStringArray(
        pos: Position,
        sep: Val.Str,
        direct: Array[Eval],
        len: Int): Val.Str = {
      val sepStr = sep.str
      val sepLen = sepStr.length
      var totalLen = 0L
      var elemCount = 0
      var asciiSafe = true
      var i = 0
      // Pass 1: validate element types, accumulate total char length and asciiSafe.
      while (i < len) {
        direct(i) match {
          case _: Val.Null =>
          case x: Val.Str  =>
            totalLen += x.str.length
            if (totalLen > Int.MaxValue) Error.fail("String is too large to join")
            asciiSafe &&= x.isInstanceOf[Val.AsciiSafeStr]
            elemCount += 1
          case _ => return null
        }
        i += 1
      }
      if (elemCount == 0) return Val.Str.asciiSafe(pos, "")
      if (elemCount > 1) {
        totalLen += sepLen.toLong * (elemCount - 1)
        if (totalLen > Int.MaxValue) Error.fail("String is too large to join")
        asciiSafe &&= sep.isInstanceOf[Val.AsciiSafeStr]
      }

      val b = new java.lang.StringBuilder(totalLen.toInt)
      i = 0
      var needsSep = false
      // Pass 2: append. Pass 1 already validated all non-Null entries are Val.Str, so the
      // unchecked cast below is safe and avoids a redundant pattern match dispatch.
      while (i < len) {
        val v = direct(i)
        if (!v.isInstanceOf[Val.Null]) {
          if (needsSep) b.append(sepStr)
          needsSep = true
          b.append(v.asInstanceOf[Val.Str].str)
        }
        i += 1
      }
      val result = b.toString
      if (asciiSafe) Val.Str.asciiSafe(pos, result) else Val.Str(pos, result)
    }

    def evalRhs(sep: Eval, _arr: Eval, ev: EvalScope, pos: Position): Val = {
      val arr = implicitly[ReadWriter[Val.Arr]].apply(_arr.value)
      sep.value match {
        case sepStr: Val.Str =>
          val len = arr.length
          val s = sepStr.str
          val repeatedConst = joinRepeatedStringEval(pos, sepStr, arr.constantEval, len)
          if (repeatedConst != null) return repeatedConst

          if (len == 0) return Val.Str.asciiSafe(pos, "")

          val direct = arr.directBackingArray
          if (direct != null) {
            val repeated = joinRepeatedDirectString(pos, sepStr, direct, len)
            if (repeated != null) return repeated

            val joined = joinDirectStringArray(pos, sepStr, direct, len)
            if (joined != null) return joined
          }

          if (len >= PresizedStringJoinMinParts) {
            return joinPresizedStringArray(pos, sepStr, arr, len)
          }

          val b = new java.lang.StringBuilder()
          var i = 0
          var added = false
          var asciiSafe = true
          while (i < len) {
            arr.value(i) match {
              case _: Val.Null =>
              case x: Val.Str  =>
                if (added) {
                  b.append(s)
                  asciiSafe &&= sepStr.isInstanceOf[Val.AsciiSafeStr]
                }
                added = true
                b.append(x.str)
                asciiSafe &&= x.isInstanceOf[Val.AsciiSafeStr]
              case x => Error.fail("Cannot join " + x.prettyName)
            }
            i += 1
          }
          val result = b.toString
          if (asciiSafe) Val.Str.asciiSafe(pos, result) else Val.Str(pos, result)
        case sep: Val.Arr =>
          val len = arr.length
          if (len > PresizedCopyMaxParts) {
            val out = new mutable.ArrayBuilder.ofRef[Eval]
            out.sizeHint(len * 2)
            var added = false
            var i = 0
            while (i < len) {
              arr.value(i) match {
                case Val.Null(_) => // do nothing
                case v: Val.Arr  =>
                  if (added) sep.copyEvalTo(out)
                  added = true
                  v.copyEvalTo(out)
                case x => Error.fail("Cannot join " + x.prettyName)
              }
              i += 1
            }
            return Val.Arr(pos, out.result())
          }

          val parts = new Array[Val.Arr](len)
          val sepLen = sep.length
          var partCount = 0
          var totalLen = 0L
          var i = 0
          while (i < len) {
            arr.value(i) match {
              case Val.Null(_) => // do nothing
              case v: Val.Arr  =>
                if (partCount > 0) totalLen += sepLen
                totalLen += v.length
                if (totalLen > Int.MaxValue) Error.fail("array too large", pos)(ev)
                parts(partCount) = v
                partCount += 1
              case x => Error.fail("Cannot join " + x.prettyName)
            }
            i += 1
          }

          val result = new Array[Eval](totalLen.toInt)
          var offset = 0
          i = 0
          while (i < partCount) {
            if (i > 0 && sepLen != 0) offset = sep.copyEvalTo(result, offset)
            offset = parts(i).copyEvalTo(result, offset)
            i += 1
          }
          Val.Arr(pos, result)
        case x => Error.fail("Cannot join " + x.prettyName)
      }
    }
  }

  private def splitLimit(
      pos: Position,
      str: String,
      cStr: String,
      maxSplits: Int,
      asciiSafe: Boolean): Array[Eval] = {
    if (cStr.isEmpty) {
      Error.fail("Cannot split by an empty string")
    }

    val b = new mutable.ArrayBuilder.ofRef[Eval]
    if (maxSplits >= 0 && maxSplits < Int.MaxValue) b.sizeHint(maxSplits + 1)
    var sz = 0
    var start = 0
    var next = if (maxSplits == 0) -1 else str.indexOf(cStr, start)

    while (next >= 0 && (maxSplits < 0 || sz < maxSplits)) {
      val piece = str.substring(start, next)
      b += (if (asciiSafe) Val.Str.asciiSafe(pos, piece) else Val.Str(pos, piece))
      start = next + cStr.length
      sz += 1
      next = if (maxSplits >= 0 && sz >= maxSplits) -1 else str.indexOf(cStr, start)
    }
    val tail = str.substring(start)
    b += (if (asciiSafe) Val.Str.asciiSafe(pos, tail) else Val.Str(pos, tail))
    sz += 1
    b.result()
  }

  private final val SplitLimitRPreallocMaxSplits = 4096

  @inline private def lastSplitIndex(str: String, cStr: String, cLen: Int, from: Int): Int = {
    var i = if (from < str.length - cLen) from else str.length - cLen
    if (cLen == 1) {
      val ch = cStr.charAt(0)
      while (i >= 0) {
        if (str.charAt(i) == ch) return i
        i -= 1
      }
    } else if (cLen == 2) {
      val ch0 = cStr.charAt(0)
      val ch1 = cStr.charAt(1)
      while (i >= 0) {
        if (str.charAt(i) == ch0 && str.charAt(i + 1) == ch1) return i
        i -= 1
      }
    } else {
      val ch0 = cStr.charAt(0)
      while (i >= 0) {
        if (str.charAt(i) == ch0) {
          var j = 1
          while (j < cLen && str.charAt(i + j) == cStr.charAt(j)) j += 1
          if (j == cLen) return i
        }
        i -= 1
      }
    }
    -1
  }

  private def splitLimitR(
      pos: Position,
      str: String,
      cStr: String,
      maxSplits: Int,
      asciiSafe: Boolean): Array[Eval] = {
    if (maxSplits == -1) {
      return splitLimit(pos, str, cStr, maxSplits, asciiSafe)
    }

    if (cStr.isEmpty) {
      Error.fail("Cannot split by an empty string")
    }

    if (maxSplits >= 0 && maxSplits <= SplitLimitRPreallocMaxSplits) {
      return splitLimitRBounded(pos, str, cStr, maxSplits, asciiSafe)
    }

    val b = new mutable.ArrayBuilder.ofRef[Eval]
    val cLen = cStr.length
    var sz = 0
    var end = str.length
    var next = if (maxSplits == 0) -1 else lastSplitIndex(str, cStr, cLen, end - cLen)

    while (next >= 0 && (maxSplits < 0 || sz < maxSplits)) {
      val piece = str.substring(next + cLen, end)
      b += (if (asciiSafe) Val.Str.asciiSafe(pos, piece) else Val.Str(pos, piece))
      end = next
      sz += 1
      next =
        if (maxSplits >= 0 && sz >= maxSplits) -1
        else lastSplitIndex(str, cStr, cLen, end - cLen)
    }
    val head = str.substring(0, end)
    b += (if (asciiSafe) Val.Str.asciiSafe(pos, head) else Val.Str(pos, head))

    val result = b.result()
    var left = 0
    var right = result.length - 1
    while (left < right) {
      val tmp = result(left)
      result(left) = result(right)
      result(right) = tmp
      left += 1
      right -= 1
    }
    result
  }

  private def splitLimitRBounded(
      pos: Position,
      str: String,
      cStr: String,
      maxSplits: Int,
      asciiSafe: Boolean): Array[Eval] = {
    val cLen = cStr.length
    val result = new Array[Eval](maxSplits + 1)
    var out = maxSplits
    var sz = 0
    var end = str.length
    var next = if (maxSplits == 0) -1 else lastSplitIndex(str, cStr, cLen, end - cLen)

    while (next >= 0 && sz < maxSplits) {
      val piece = str.substring(next + cLen, end)
      result(out) = if (asciiSafe) Val.Str.asciiSafe(pos, piece) else Val.Str(pos, piece)
      out -= 1
      end = next
      sz += 1
      next =
        if (sz >= maxSplits) -1
        else lastSplitIndex(str, cStr, cLen, end - cLen)
    }
    val head = str.substring(0, end)
    result(out) = if (asciiSafe) Val.Str.asciiSafe(pos, head) else Val.Str(pos, head)

    if (out == 0) result
    else java.util.Arrays.copyOfRange(result, out, maxSplits + 1)
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
      val v = str.value
      val safe = v.isInstanceOf[Val.AsciiSafeStr]
      Val.Arr(pos, splitLimit(pos, v.asString, c.value.asString, -1, safe))
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
      val v = str.value
      val safe = v.isInstanceOf[Val.AsciiSafeStr]
      Val.Arr(
        pos,
        splitLimit(pos, v.asString, c.value.asString, maxSplits.value.asInt, safe)
      )
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
      val v = str.value
      val safe = v.isInstanceOf[Val.AsciiSafeStr]
      Val.Arr(
        pos,
        splitLimitR(pos, v.asString, c.value.asString, maxSplits.value.asInt, safe)
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
      if (s.charAt(0) == '-') {
        val result = parseNat(s, 1, 10, "base 10")
        Val.cachedNum(pos, if (result == 0.0) 0.0 else -result)
      } else Val.cachedNum(pos, parseNat(s, 0, 10, "base 10"))
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
    val len = str.length
    // Fast path for base 10: process 4 digits at a time.
    // Inspired by jsoniter-scala's SWAR digit parsing technique.
    if (base == 10 && len - start >= 4) {
      val limit = len - 3
      while (i < limit) {
        val c0 = str.charAt(i).toInt - '0'
        val c1 = str.charAt(i + 1).toInt - '0'
        val c2 = str.charAt(i + 2).toInt - '0'
        val c3 = str.charAt(i + 3).toInt - '0'
        if (c0 < 0 || c0 > 9 || c1 < 0 || c1 > 9 || c2 < 0 || c2 > 9 || c3 < 0 || c3 > 9) {
          // Non-digit found, fall through to scalar loop for error reporting
          while (i < len) {
            val digit = str.charAt(i) - '0'
            if (digit < 0 || digit >= base) {
              Error.fail("Cannot parse '" + str + "' as an integer in " + baseName)
            }
            acc = base * acc + digit
            i += 1
          }
          return acc
        }
        acc = acc * 10000 + c0 * 1000 + c1 * 100 + c2 * 10 + c3
        i += 4
      }
    }
    // Scalar tail: process remaining digits one at a time
    while (i < len) {
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
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
      val v = str.value
      val s = v.asString
      val out = asciiUpper(s)
      v match {
        case _: Val.AsciiSafeStr => Val.Str.asciiSafe(pos, out)
        case _                   => Val.Str(pos, out)
      }
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-asciiLower std.asciiLower(str)]].
   *
   * Since: 0.10.0. Group: String Manipulation.
   *
   * Returns a copy of the string in which all ASCII letters are lower cased.
   */
  private object AsciiLower extends Val.Builtin1("asciiLower", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
      val v = str.value
      val s = v.asString
      val out = asciiLower(s)
      v match {
        case _: Val.AsciiSafeStr => Val.Str.asciiSafe(pos, out)
        case _                   => Val.Str(pos, out)
      }
    }
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
      Format.format(str.value.asString, vals.value, pos)(ev)
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
    builtin("isEmpty", "str") { (pos, ev, value: Val) =>
      value match {
        case Val.Str(_, s) => s.isEmpty
        case a: Val.Arr    => a.length == 0
        case o: Val.Obj    => o.visibleKeyNames.isEmpty
        case x             =>
          Error.fail(
            "isEmpty operates on strings, objects, and arrays, got " + x.prettyName,
            pos
          )(ev)
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
      val v = str.value
      v match {
        case s: Val.Str =>
          val out = new StringBuilderWriter(s.str.length + 16)
          BaseRenderer.escape(out, s.str, unicode = true)
          out.toString
        case _ =>
          val out = new StringBuilderWriter(64)
          BaseRenderer.escape(out, Materializer.stringify(v)(ev), unicode = true)
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
      val s = stdToString(str)(ev)
      val out = new StringBuilderWriter(s.length + 16)
      BaseRenderer.escape(out, s, unicode = true)
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
      val out = new StringBuilderWriter(string.length + 16)
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
