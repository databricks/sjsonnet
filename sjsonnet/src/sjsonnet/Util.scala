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
}