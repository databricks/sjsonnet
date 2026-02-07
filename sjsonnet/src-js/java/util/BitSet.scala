package java.util

import java.util
import java.util.stream.IntStream
import scala.collection.mutable.{BitSet => SBitSet}

class BitSet(_initialSizeDummy: Int) extends java.lang.Cloneable {
  val bs: SBitSet = SBitSet.empty
  def isEmpty: Boolean = bs.isEmpty
  override def clone(): AnyRef = super.clone()
  def cardinality(): Int = bs.size
  def stream(): IntStream = IntStream.of(bs.toArray: _*)
  def get(i: Int): Boolean = bs.apply(i)
  def set(i: Int): Unit = bs += i
  def clear(): Unit = bs.clear()
  def andNot(other: util.BitSet): Unit = bs.&~=(other.bs)
}
