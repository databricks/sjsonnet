package sjsonnet

import ujson.Value
import utest._

object BinarySearchTests extends TestSuite{
  def check(arr: Array[Int], parserInput: fastparse.IndexedParserInput, i: Int) = {
    val scanned = parserInput.prettyIndex(i)
    val searched = Util.prettyIndex(arr, i)
    assert(searched == scanned)
  }
  def initData(n: Int) = {
    val random = new scala.util.Random(n)
    val str = Array.fill(n)(random.nextInt(n)).map("x" * _).mkString("\n")
    val arr = fastparse.internal.Util.lineNumberLookup(str)
    val parserInput = new fastparse.IndexedParserInput(str)
    (arr, parserInput)
  }
  def tests = Tests{
    test("five"){
      val (arr, parserInput) = initData(5)
      assert(arr.last < 20)

      test{ check(arr, parserInput, 0) }
      test{ check(arr, parserInput, 1) }
      test{ check(arr, parserInput, 2) }
      test{ check(arr, parserInput, 3) }
      test{ check(arr, parserInput, 4) }
      test{ check(arr, parserInput, 5) }
      test{ check(arr, parserInput, 6) }
      test{ check(arr, parserInput, 7) }
      test{ check(arr, parserInput, 8) }
      test{ check(arr, parserInput, 9) }
      test{ check(arr, parserInput, 10) }
      test{ check(arr, parserInput, 11) }
      test{ check(arr, parserInput, 12) }
      test{ check(arr, parserInput, 13) }
      test{ check(arr, parserInput, 14) }
      test{ check(arr, parserInput, 15) }
      test{ check(arr, parserInput, 16) }
      test{ check(arr, parserInput, 17) }
      test{ check(arr, parserInput, 18) }
      test{ check(arr, parserInput, 19) }
      test{ check(arr, parserInput, 20) }

    }
    test("ten"){
      val (arr, parserInput) = initData(10)
      assert(arr.last < 50)

      test{ check(arr, parserInput, 0) }
      test{ check(arr, parserInput, 1) }
      test{ check(arr, parserInput, 2) }
      test{ check(arr, parserInput, 3) }
      test{ check(arr, parserInput, 4) }
      test{ check(arr, parserInput, 5) }
      test{ check(arr, parserInput, 6) }
      test{ check(arr, parserInput, 7) }
      test{ check(arr, parserInput, 8) }
      test{ check(arr, parserInput, 9) }

      test{ check(arr, parserInput, 10) }
      test{ check(arr, parserInput, 11) }
      test{ check(arr, parserInput, 12) }
      test{ check(arr, parserInput, 13) }
      test{ check(arr, parserInput, 14) }
      test{ check(arr, parserInput, 15) }
      test{ check(arr, parserInput, 16) }
      test{ check(arr, parserInput, 17) }
      test{ check(arr, parserInput, 18) }
      test{ check(arr, parserInput, 19) }

      test{ check(arr, parserInput, 20) }
      test{ check(arr, parserInput, 21) }
      test{ check(arr, parserInput, 22) }
      test{ check(arr, parserInput, 23) }
      test{ check(arr, parserInput, 24) }
      test{ check(arr, parserInput, 25) }
      test{ check(arr, parserInput, 26) }
      test{ check(arr, parserInput, 27) }
      test{ check(arr, parserInput, 28) }
      test{ check(arr, parserInput, 29) }

      test{ check(arr, parserInput, 30) }
      test{ check(arr, parserInput, 31) }
      test{ check(arr, parserInput, 32) }
      test{ check(arr, parserInput, 33) }
      test{ check(arr, parserInput, 34) }
      test{ check(arr, parserInput, 35) }
      test{ check(arr, parserInput, 36) }
      test{ check(arr, parserInput, 37) }
      test{ check(arr, parserInput, 38) }
      test{ check(arr, parserInput, 39) }

      test{ check(arr, parserInput, 40) }
      test{ check(arr, parserInput, 41) }
      test{ check(arr, parserInput, 42) }
      test{ check(arr, parserInput, 43) }
      test{ check(arr, parserInput, 44) }
      test{ check(arr, parserInput, 45) }
      test{ check(arr, parserInput, 46) }
      test{ check(arr, parserInput, 47) }
      test{ check(arr, parserInput, 48) }
      test{ check(arr, parserInput, 49) }
    }
    test("hundred") {
      val (arr, parserInput) = initData(100)
      for (i <- Range(0, arr.max + 100)) check(arr, parserInput, i)
    }
    test("fivehundred") {
      val (arr, parserInput) = initData(500)
      for (i <- Range(0, arr.max + 500)) check(arr, parserInput, i)
    }
  }
}
