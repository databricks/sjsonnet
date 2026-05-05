package sjsonnet

import utest._

object ValArrayViewTests extends TestSuite {
  private val testPos = new Position(null, -1)

  private final class NoBulkArr(size: Int) extends Val.Arr(testPos, null) {
    var bulkCalls: Int = 0

    override def length: Int = size

    override def value(i: Int): Val = Val.cachedNum(testPos, i)

    override def eval(i: Int): Eval = Val.cachedNum(testPos, i)

    override def asLazyArray: Array[Eval] = {
      bulkCalls += 1
      throw new java.lang.AssertionError("source asLazyArray should not be required")
    }
  }

  def tests: Tests = Tests {
    test("sliceAvoidsBulkMaterializingSource") {
      val source = new NoBulkArr(10000)

      val small = source.sliced(testPos, 10, 16, 2)
      small.asStrictArray.map(_.asDouble).toSeq ==> Seq(10.0, 12.0, 14.0)
      source.bulkCalls ==> 0

      val large = source.sliced(testPos, 100, 9000, 3)
      large.length ==> 2967
      large.value(0).asDouble ==> 100.0
      large.value(1).asDouble ==> 103.0
      large.value(large.length - 1).asDouble ==> 8998.0

      val reversed = large.reversed(testPos)
      reversed.value(0).asDouble ==> 8998.0
      reversed.value(reversed.length - 1).asDouble ==> 100.0

      val materialized = large.asLazyArray
      materialized.length ==> 2967
      materialized(2).value.asDouble ==> 106.0
      source.bulkCalls ==> 0
    }

    test("copyEvalToAvoidsBulkMaterializingConcatChildren") {
      val left = new NoBulkArr(5000)
      val right = new NoBulkArr(5000)
      val concat = left.concat(testPos, right)

      val result = new Array[Eval](concat.length)
      concat.copyEvalTo(result, 0) ==> 10000

      result(0).value.asDouble ==> 0.0
      result(4999).value.asDouble ==> 4999.0
      result(5000).value.asDouble ==> 0.0
      result(9999).value.asDouble ==> 4999.0
      left.bulkCalls ==> 0
      right.bulkCalls ==> 0
    }
  }
}
