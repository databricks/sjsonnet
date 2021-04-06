package sjsonnet

import java.util.BitSet

object BitSetUtils {
  def iterator(bs: BitSet): Iterator[Int] = bs.bs.iterator
}
