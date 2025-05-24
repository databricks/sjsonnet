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

  def sliceArr[T: scala.reflect.ClassTag](
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
      case Val.Str(_, s) => s.length
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
      case None => 1
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

  def sliceArr[T: scala.reflect.ClassTag](
      arr: Array[T],
      start: Option[Int],
      end: Option[Int],
      step: Option[Int]): Array[T] = {
    sliceArr(arr, start.getOrElse(0), end.getOrElse(arr.length), step.getOrElse(1))
  }
  def sliceStr(s: String, start: Int, end: Int, step: Int): String = {
    if (start >= end || start >= s.length) {
      ""
    } else
      step match {
        case 1 => s.slice(start, end)
        case _ =>
          val range = start until end by step
          new String(range.dropWhile(_ < 0).takeWhile(_ < s.length).map(s).toArray)
      }
  }
  def sliceStr(s: String, start: Option[Int], end: Option[Int], step: Option[Int]): String = {
    sliceStr(s, start.getOrElse(0), end.getOrElse(s.length), step.getOrElse(1))
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
