package sjsonnet

object Util {
  private val hashMapDefaultLoadFactor = 0.75f
  def prettyIndex(lineStarts: Array[Int], index: Int): String = {
    // Returns (-insertionPoint - 1) when the value is not found, where
    // insertionPoint is the index where the element would have been inserted.
    // Since we want to find the index of the line before the insertion point
    // (insertionPoint - 1), we need to do a bit of math to get it
    //
    // (-insertionPoint - 1) = searchResult
    // (insertionPoint + 1) = -searchResult
    // (insertionPoint - 1) = -searchResult - 2
    val searchResult = java.util.Arrays.binarySearch(lineStarts, index)
    val line = if (searchResult >= 0) searchResult else -searchResult - 2
    val col = index - lineStarts(line)
    s"${line + 1}:${col + 1}"
  }

  private def sliceArr[T: scala.reflect.ClassTag](
      arr: Array[T],
      start: Int,
      end: Int,
      step: Int): Array[T] = {
    if (start >= end || start >= arr.length) {
      Array.empty[T]
    } else
      step match {
        case 1 => arr.slice(start, end)
        case _ =>
          val range = start until end by step
          range.dropWhile(_ < 0).takeWhile(_ < arr.length).map(arr).toArray
      }
  }

  def slice(
      pos: Position,
      ev: EvalScope,
      indexable: Val,
      index: Option[Int],
      _end: Option[Int],
      _step: Option[Int]): Val = {
    def length0(e: Val): Int = e match {
      case Val.Str(_, s) => s.codePointCount(0, s.length)
      case a: Val.Arr    => a.length
      case x             => Error.fail("Cannot get length of " + x.prettyName, e.pos)(ev)
    }
    val length = length0(indexable)
    val start = index match {
      case None    => 0
      case Some(i) => if (i < 0) Math.max(0, length + i) else i
    }
    val end = _end match {
      case None    => length
      case Some(e) => if (e < 0) length + e else e
    }
    val step = _step match {
      case None    => 1
      case Some(s) =>
        if (s < 0) {
          Error.fail(s"got [$start:$end:$s] but negative steps are not supported", pos)(ev)
        } else if (s == 0) {
          Error.fail(s"got $s but step must be greater than 0", pos)(ev)
        }
        s
    }
    val res = indexable match {
      case Val.Str(_, s) => Val.Str(pos, Util.sliceStr(s, start, end, step))
      case arr: Val.Arr  => Val.Arr(pos, Util.sliceArr(arr.asLazyArray, start, end, step))
      case _ => Error.fail("Can only slice array or string, not " + indexable.prettyName, pos)(ev)
    }
    res: Val
  }

  /**
   * Converts Unicode codepoint positions to Java String indices. For example, the string "🌍!" has
   * a length of 3 UTF-16 code units, but only 2 Unicode codepoints, so this function would map the
   * range (0, 2) to (0, 3).
   */
  def codePointOffsetsToStringIndices(
      s: String,
      startCodePointOffset: Int,
      endCodePointOffset: Int): (Int, Int) = {
    val unicodeLength = s.codePointCount(0, s.length)
    val safeStart = math.max(0, math.min(startCodePointOffset, unicodeLength))
    val safeEnd = math.max(safeStart, math.min(endCodePointOffset, unicodeLength))

    if (safeStart == safeEnd) {
      val utf16Pos = if (safeStart == 0) 0 else s.offsetByCodePoints(0, safeStart)
      (utf16Pos, utf16Pos)
    } else {
      val startUtf16 = if (safeStart == 0) 0 else s.offsetByCodePoints(0, safeStart)
      val endUtf16 = s.offsetByCodePoints(startUtf16, safeEnd - safeStart)
      (startUtf16, endUtf16)
    }
  }

  private def sliceStr(s: String, start: Int, end: Int, step: Int): String = {
    val unicodeLength = s.codePointCount(0, s.length)
    if (start >= end || start >= unicodeLength) {
      ""
    } else {
      step match {
        case 1 =>
          val (startUtf16, endUtf16) = codePointOffsetsToStringIndices(s, start, end)
          s.substring(startUtf16, endUtf16)
        case _ =>
          val result =
            new java.lang.StringBuilder(math.min(s.length, ((end - start) + step - 1) / step))
          var sIdx = 0
          var codepointIndex = 0

          // Skip to start codepoint position
          while (sIdx < s.length && codepointIndex < start) {
            val cp = s.codePointAt(sIdx)
            sIdx += Character.charCount(cp)
            codepointIndex += 1
          }

          // Collect every `step`th codepoint until `end`
          var rel = 0 // relative index from start
          var nextInclude = 0 // next relative index to include
          while (sIdx < s.length && codepointIndex < end) {
            val c = s.charAt(sIdx)
            if (Character.isSurrogate(c)) {
              // Handle surrogate pair (or unpaired surrogates)
              val cp = s.codePointAt(sIdx)
              if (rel == nextInclude) {
                result.append(Character.toString(cp))
                nextInclude += step
              }
              sIdx += Character.charCount(cp)
            } else {
              // Single char, non-surrogate
              if (rel == nextInclude) {
                result.append(c)
                nextInclude += step
              }
              sIdx += 1
            }
            codepointIndex += 1
            rel += 1
          }
          result.toString
      }
    }
  }

  /**
   * Compares two strings by Unicode codepoint values rather than UTF-16 code units. This ensures
   * that strings with characters above U+FFFF (which require surrogate pairs in UTF-16) are
   * compared correctly according to their Unicode codepoint values.
   */
  def compareStringsByCodepoint(s1: String, s2: String): Int = {
    val n1 = s1.length
    val n2 = s2.length
    var i1 = 0
    var i2 = 0
    while (i1 < n1 && i2 < n2) {
      val c1 = s1.charAt(i1)
      val c2 = s2.charAt(i2)
      val c1Sur = Character.isSurrogate(c1)
      val c2Sur = Character.isSurrogate(c2)

      if (!c1Sur && !c2Sur) {
        // Both are non-surrogates, compare directly
        if (c1 != c2) return Character.compare(c1, c2)
        i1 += 1
        i2 += 1
      } else {
        // At least one is a surrogate, use full codepoint logic
        val cp1 = s1.codePointAt(i1)
        val cp2 = s2.codePointAt(i2)
        if (cp1 != cp2) return Integer.compare(cp1, cp2)
        i1 += Character.charCount(cp1)
        i2 += Character.charCount(cp2)
      }
    }
    if (i1 < n1) 1 else if (i2 < n2) -1 else 0
  }

  /**
   * A reusable Ordering[String] that compares by Unicode codepoint values. Use this in place of
   * default `.sorted` when ordering should be codepoint-aware.
   */
  val CodepointStringOrdering: Ordering[String] = new Ordering[String] {
    override def compare(x: String, y: String): Int = compareStringsByCodepoint(x, y)
  }

  val isWindows: Boolean = {
    // This is normally non-null on the JVM, but it might be null in ScalaJS hence the Option:
    Option(System.getProperty("os.name")).exists(_.toLowerCase.startsWith("windows"))
  }

  /**
   * Wrap the given string in '<' and '>' brackets for pretty printing. On Windows, this uses
   * Unicode less-than and greater-than characters, while on other platforms it uses ASCII '<' and
   * '>; see https://github.com/databricks/sjsonnet/pull/208 for motivation and context.
   */
  def wrapInLessThanGreaterThan(s: String): String = {
    if (isWindows) {
      s"\uFE64$s\uFE65"
    } else {
      s"<$s>"
    }
  }

  def preSizedJavaLinkedHashMap[K, V](expectedElems: Int): java.util.LinkedHashMap[K, V] = {
    new java.util.LinkedHashMap[K, V](hashMapCapacity(expectedElems), hashMapDefaultLoadFactor)
  }

  def preSizedJavaHashMap[K, V](expectedElems: Int): java.util.HashMap[K, V] = {
    new java.util.HashMap[K, V](hashMapCapacity(expectedElems), hashMapDefaultLoadFactor)
  }

  private def hashMapCapacity(expectedElems: Int): Int = {
    if (expectedElems < 3) {
      expectedElems + 1
    } else {
      // Set the initial capacity to the number of elems divided by the default load factor + 1
      // this ensures that we can fill up the map to the total number of fields without resizing.
      // From JavaDoc - true for both Scala & Java HashMaps
      (expectedElems / hashMapDefaultLoadFactor).toInt + 1
    }
  }
}
