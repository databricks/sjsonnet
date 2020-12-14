package sjsonnet


object Util{
  def binarySearch(lineStartsMin: Int, lineStartsMax: Int, lineStarts: Array[Int], index: Int): Int = {
    if (lineStartsMin == lineStartsMax - 1) lineStartsMin
    else {
      val middle = (lineStartsMax + lineStartsMin) / 2
      if (lineStarts(middle) > index) binarySearch(lineStartsMin, middle, lineStarts, index)
      else binarySearch(middle, lineStartsMax, lineStarts, index)
    }
  }

  def prettyIndex(lineStarts: Array[Int], index: Int): String = {
    val line = Util.binarySearch(0, lineStarts.length, lineStarts, index)
    val col = index - lineStarts(line)
    s"${line+1}:${col+1}"
  }
}