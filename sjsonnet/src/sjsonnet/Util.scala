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
}