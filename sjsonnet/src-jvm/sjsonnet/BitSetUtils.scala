package sjsonnet

import java.util.BitSet

import scala.collection.mutable.ArrayBuffer

object BitSetUtils {
  def iterator(bs: BitSet): Iterator[Int] = {
    val b = ArrayBuffer.empty[Int]
    var i = bs.nextSetBit(0)
    while(i  > 0) {
      b += i
      i = bs.nextSetBit(i+1)
    }
    b.iterator
  }
}
