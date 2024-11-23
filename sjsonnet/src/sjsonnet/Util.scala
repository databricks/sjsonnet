package sjsonnet


object Util{
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
    s"${line+1}:${col+1}"
  }

  def sliceArr[T: scala.reflect.ClassTag](arr: Array[T], start: Int, end: Int, step: Int): Array[T] = {
    step match{
      case 1 => arr.slice(start, end)
      case _ =>
        val range = start until end by step
        range.dropWhile(_ < 0).takeWhile(_ < arr.length).map(arr).toArray
    }
  }
  def sliceStr(s: String, start: Int, end: Int, step: Int): String = {
    step match{
      case 1 => s.slice(start, end)
      case _ =>
        val range = start until end by step
        new String(range.dropWhile(_ < 0).takeWhile(_ < s.length).map(s).toArray)
    }
  }

  val isWindows: Boolean = {
    // This is normally non-null on the JVM, but it might be null in ScalaJS hence the Option:
    Option(System.getProperty("os.name")).exists(_.toLowerCase.startsWith("windows"))
  }

  /**
   * Wrap the given string in '<' and '>' brackets for pretty printing.
   * On Windows, this uses Unicode less-than and greater-than characters, while on
   * other platforms it uses ASCII '<' and '>;
   * see https://github.com/databricks/sjsonnet/pull/208 for motivation and context.
   */
  def wrapInLessThanGreaterThan(s: String): String = {
    if (isWindows) {
      s"\uFE64$s\uFE65"
    } else {
      s"<$s>"
    }
  }

  def preSizedJavaLinkedHashMap[K, V](expectedElems: Int): java.util.LinkedHashMap[K, V] = {
    // Set the initial capacity to the number of elems divided by the default load factor + 1
    // this ensures that we can fill up the map to the total number of fields without resizing.
    // From JavaDoc - true for both Scala & Java HashMaps
    val hashMapDefaultLoadFactor = 0.75f
    val capacity = (expectedElems / hashMapDefaultLoadFactor).toInt + 1
    new java.util.LinkedHashMap[K, V](capacity, hashMapDefaultLoadFactor)
  }
}
