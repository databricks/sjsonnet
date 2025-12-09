package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable

object StringModule extends AbstractFunctionModule {
  def name = "string"

  private val whiteSpaces = StripUtils.codePointsSet(" \t\n\f\r\u0085\u00A0")

  private object ToString extends Val.Builtin1("toString", "a") {
    def evalRhs(v1: Lazy, ev: EvalScope, pos: Position): Val = Val.Str(
      pos,
      v1.force match {
        case Val.Str(_, s) => s
        case v             => Materializer.stringify(v)(ev)
      }
    )
  }

  private object Length extends Val.Builtin1("length", "x") {
    def evalRhs(x: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Num(
        pos,
        x.force match {
          case Val.Str(_, s) => s.codePointCount(0, s.length)
          case a: Val.Arr    => a.length
          case o: Val.Obj    => o.visibleKeyNames.length
          case o: Val.Func   => o.params.names.length
          case x             => Error.fail("Cannot get length of " + x.prettyName)
        }
      )
  }

  private object Codepoint extends Val.Builtin1("codepoint", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val = {
      val s = str.force.asString
      val codePointCount = s.codePointCount(0, s.length)
      if (codePointCount != 1) {
        Error.fail("expected a single character string, got " + s)
      } else {
        Val.Num(pos, s.codePointAt(0).toDouble)
      }
    }
  }

  private object Substr extends Val.Builtin3("substr", "str", "from", "len") {
    def evalRhs(_s: Lazy, from: Lazy, len: Lazy, ev: EvalScope, pos: Position): Val = {
      val str = _s.force.asString
      val offset = from.force match {
        case v: Val.Num => v.asPositiveInt
        case _ => Error.fail("Expected a number for offset in substr, got " + from.force.prettyName)
      }
      val length = len.force match {
        case v: Val.Num => v.asPositiveInt
        case _ => Error.fail("Expected a number for len in substr, got " + len.force.prettyName)
      }

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

  private object StartsWith extends Val.Builtin2("startsWith", "a", "b") {
    def evalRhs(a: Lazy, b: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, a.force.asString.startsWith(b.force.asString))
  }

  private object EndsWith extends Val.Builtin2("endsWith", "a", "b") {
    def evalRhs(a: Lazy, b: Lazy, ev: EvalScope, pos: Position): Val =
      Val.bool(pos, a.force.asString.endsWith(b.force.asString))
  }

  private object Char_ extends Val.Builtin1("char", "n") {
    def evalRhs(n: Lazy, ev: EvalScope, pos: Position): Val = {
      val c = n.force.asInt
      if (!Character.isValidCodePoint(c)) {
        Error.fail(s"Invalid unicode code point, got " + c)
      }
      Val.Str(pos, Character.toString(c))
    }
  }

  private object StrReplace extends Val.Builtin3("strReplace", "str", "from", "to") {
    def evalRhs(str: Lazy, from: Lazy, to: Lazy, ev: EvalScope, pos: Position): Val = {
      val fromForce = from.force.asString
      if (fromForce.isEmpty) {
        Error.fail("Cannot replace empty string in strReplace")
      }
      Val.Str(pos, str.force.asString.replace(fromForce, to.force.asString))
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

    def unspecializedStrip(
        str: String,
        charsSet: collection.Set[Int],
        left: Boolean,
        right: Boolean): String = {
      if (str.isEmpty) return str
      var start = 0
      // Use exclusive end position with codePointBefore() for right-to-left iteration.
      // Unlike codePointAt(), codePointBefore() correctly reads surrogate pairs when
      // scanning backwards (codePointAt on a low surrogate returns the wrong value).
      var end = str.length

      while (left && start < end && charsSet.contains(str.codePointAt(start))) {
        start = str.offsetByCodePoints(start, 1)
      }

      while (right && end > start && charsSet.contains(str.codePointBefore(end))) {
        end = str.offsetByCodePoints(end, -1)
      }
      str.substring(start, end)
    }
  }

  private object StripChars extends Val.Builtin2("stripChars", "str", "chars") {
    def evalRhs(str: Lazy, chars: Lazy, ev: EvalScope, pos: Position): Val = {
      val charsSet = StripUtils.codePointsSet(chars.force.asString)
      Val.Str(
        pos,
        StripUtils.unspecializedStrip(
          str.force.asString,
          charsSet,
          left = true,
          right = true
        )
      )
    }
  }

  private object LStripChars extends Val.Builtin2("lstripChars", "str", "chars") {
    def evalRhs(str: Lazy, chars: Lazy, ev: EvalScope, pos: Position): Val = {
      val charsSet = StripUtils.codePointsSet(chars.force.asString)
      Val.Str(
        pos,
        StripUtils.unspecializedStrip(
          str.force.asString,
          charsSet,
          left = true,
          right = false
        )
      )
    }
  }

  private object RStripChars extends Val.Builtin2("rstripChars", "str", "chars") {
    def evalRhs(str: Lazy, chars: Lazy, ev: EvalScope, pos: Position): Val = {
      val charsSet = StripUtils.codePointsSet(chars.force.asString)
      Val.Str(
        pos,
        StripUtils.unspecializedStrip(
          str.force.asString,
          charsSet,
          left = false,
          right = true
        )
      )
    }
  }

  private object Join extends Val.Builtin2("join", "sep", "arr") {
    def evalRhs(sep: Lazy, _arr: Lazy, ev: EvalScope, pos: Position): Val = {
      val arr = implicitly[ReadWriter[Val.Arr]].apply(_arr.force)
      sep.force match {
        case Val.Str(_, s) =>
          val b = new java.lang.StringBuilder()
          var i = 0
          var added = false
          while (i < arr.length) {
            arr.force(i) match {
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
          val out = new mutable.ArrayBuilder.ofRef[Lazy]
          // Set a reasonable size hint based on estimated result size
          out.sizeHint(arr.length * 2)
          var added = false
          for (x <- arr) {
            x match {
              case Val.Null(_) => // do nothing
              case v: Val.Arr  =>
                if (added) out ++= sep.asLazyArray
                added = true
                out ++= v.asLazyArray
              case x => Error.fail("Cannot join " + x.prettyName)
            }
          }
          Val.Arr(pos, out.result())
        case x => Error.fail("Cannot join " + x.prettyName)
      }
    }
  }

  private def splitLimit(pos: Position, str: String, cStr: String, maxSplits: Int): Array[Lazy] = {
    if (maxSplits < 0 && maxSplits != -1) {
      Error.fail("maxSplits should be -1 or non-negative, got " + maxSplits)
    }
    if (cStr.isEmpty) {
      Error.fail("Cannot split by an empty string")
    }

    val b = new mutable.ArrayBuilder.ofRef[Lazy]
    var sz = 0
    var i = 0
    var start = 0

    while (i <= str.length - cStr.length && (maxSplits < 0 || sz < maxSplits)) {
      if (str.startsWith(cStr, i)) {
        val finalStr = Val.Str(pos, str.substring(start, i))
        b.+=(finalStr)
        start = i + cStr.length
        sz += 1
        i += cStr.length
      } else {
        i += 1
      }
    }
    b.+=(Val.Str(pos, str.substring(start)))
    sz += 1
    b.result()
  }

  private object Split extends Val.Builtin2("split", "str", "c") {
    def evalRhs(str: Lazy, c: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.Arr(pos, splitLimit(pos, str.force.asString, c.force.asString, -1))
    }
  }

  private object SplitLimit extends Val.Builtin3("splitLimit", "str", "c", "maxsplits") {
    def evalRhs(str: Lazy, c: Lazy, maxSplits: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.Arr(pos, splitLimit(pos, str.force.asString, c.force.asString, maxSplits.force.asInt))
    }
  }

  private object SplitLimitR extends Val.Builtin3("splitLimitR", "str", "c", "maxsplits") {
    def evalRhs(str: Lazy, c: Lazy, maxSplits: Lazy, ev: EvalScope, pos: Position): Val = {
      Val.Arr(
        pos,
        splitLimit(pos, str.force.asString.reverse, c.force.asString.reverse, maxSplits.force.asInt)
          .map(s => Val.Str(pos, s.force.force.asString.reverse))
          .reverse
      )
    }
  }

  private object StringChars extends Val.Builtin1("stringChars", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      stringChars(pos, str.force.asString)
  }

  private object ParseInt extends Val.Builtin1("parseInt", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      try {
        Val.Num(pos, str.force.asString.toLong.toDouble)
      } catch {
        case _: NumberFormatException =>
          Error.fail("Cannot parse '" + str.force.asString + "' as an integer in base 10")
      }
  }

  private object ParseOctal extends Val.Builtin1("parseOctal", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, java.lang.Long.parseLong(str.force.asString, 8).toDouble)
  }

  private object ParseHex extends Val.Builtin1("parseHex", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Num(pos, java.lang.Long.parseLong(str.force.asString, 16).toDouble)
  }

  private object AsciiUpper extends Val.Builtin1("asciiUpper", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.force.asString.toUpperCase)
  }

  private object AsciiLower extends Val.Builtin1("asciiLower", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, str.force.asString.toLowerCase)
  }

  private object EncodeUTF8 extends Val.Builtin1("encodeUTF8", "str") {
    def evalRhs(s: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Arr(pos, s.force.asString.getBytes(UTF_8).map(i => Val.Num(pos, i & 0xff)))
  }

  private object DecodeUTF8 extends Val.Builtin1("decodeUTF8", "arr") {
    def evalRhs(arr: Lazy, ev: EvalScope, pos: Position): Val = {
      for ((v, idx) <- arr.force.asArr.iterator.zipWithIndex) {
        if (!v.isInstanceOf[Val.Num] || !v.asDouble.isWhole || v.asInt < 0 || v.asInt > 255) {
          throw Error.fail(
            f"Element $idx of the provided array was not an integer in range [0,255]"
          )
        }
      }
      Val.Str(
        pos,
        new String(arr.force.asArr.iterator.map(_.asInt.toByte).toArray, UTF_8)
      )
    }
  }

  private object Format_ extends Val.Builtin2("format", "str", "vals") {
    def evalRhs(str: Lazy, vals: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Format.format(str.force.asString, vals.force, pos)(ev))
    override def specialize(args: Array[Expr], tailstrict: Boolean): (Val.Builtin, Array[Expr]) =
      args match {
        case Array(str, fmt: Val.Str) =>
          try { (new Format.PartialApplyFmt(fmt.value), Array(str)) }
          catch { case _: Exception => null }
        case _ => null
      }
  }

  private def stringChars(pos: Position, str: String): Val.Arr = {
    val chars = new Array[Lazy](str.codePointCount(0, str.length))
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
    builtin(StringChars),
    builtin(ParseInt),
    builtin(ParseOctal),
    builtin(ParseHex),
    builtin(AsciiUpper),
    builtin(AsciiLower),
    builtin(EncodeUTF8),
    builtin(DecodeUTF8),
    builtin(Format_),
    builtin("findSubstr", "pat", "str") { (pos, ev, pat: String, str: String) =>
      if (pat.isEmpty) Val.Arr(pos, new Array[Lazy](0))
      else {
        var matchIndex = str.indexOf(pat)
        if (matchIndex == -1) Val.Arr(pos, new Array[Lazy](0))
        else {
          val indices = new mutable.ArrayBuilder.ofRef[Val.Num]

          // Compute codepoint indices incrementally, avoiding an O(n) calculation for each match.
          var prevCharIndex = 0
          var prevCodePointIndex = 0

          while (0 <= matchIndex && matchIndex < str.length) {
            val codePointIndex = prevCodePointIndex + str.codePointCount(prevCharIndex, matchIndex)
            indices.+=(Val.Num(pos, codePointIndex))

            prevCharIndex = matchIndex
            prevCodePointIndex = codePointIndex
            matchIndex = str.indexOf(pat, matchIndex + 1)
          }
          Val.Arr(pos, indices.result())
        }
      }
    },
    builtin("isEmpty", "str") { (_, _, str: String) =>
      str.isEmpty
    },
    builtin("trim", "str") { (_, _, str: String) =>
      StripUtils.unspecializedStrip(str, whiteSpaces, true, true)
    },
    builtin("equalsIgnoreCase", "str1", "str2") { (_, _, str1: String, str2: String) =>
      str1.equalsIgnoreCase(str2)
    },
    builtin("escapeStringJson", "str_") { (pos, ev, str: Val) =>
      if (str.force.isInstanceOf[Val.Str]) {
        Materializer.stringify(str)(ev)
      } else {
        val out = new java.io.StringWriter()
        BaseRenderer.escape(out, Materializer.stringify(str)(ev), unicode = true)
        out.toString
      }
    },
    builtin("escapeStringPython", "str") { (pos, ev, str: String) =>
      val out = new java.io.StringWriter()
      BaseRenderer.escape(out, str, unicode = true)
      out.toString
    },
    builtin("escapeStringXML", "str") { (_, _, str: String) =>
      val out = new java.io.StringWriter()
      for (c <- str) {
        c match {
          case '<'  => out.write("&lt;")
          case '>'  => out.write("&gt;")
          case '&'  => out.write("&amp;")
          case '"'  => out.write("&quot;")
          case '\'' => out.write("&apos;")
          case _    => out.write(c)
        }
      }
      out.toString
    },
    builtin("escapeStringBash", "str_") { (pos, ev, str: String) =>
      "'" + str.replace("'", """'"'"'""") + "'"
    },
    builtin("escapeStringDollars", "str_") { (pos, ev, str: String) =>
      str.replace("$", "$$")
    }
  )
}
