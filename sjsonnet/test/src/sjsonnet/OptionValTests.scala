package sjsonnet

import utest._

object OptionValTests extends TestSuite {
  def tests: Tests = Tests {
    test("some stores non-null references") {
      val value = new String("value")
      val option = OptionVal.some(value)
      assert(option.isDefined)
      assert(!option.isEmpty)
      assert(option.get eq value)
      assert(option.getOrElse("fallback") eq value)
    }

    test("none widens to any reference payload type") {
      val option: OptionVal[String] = OptionVal.None
      assert(option.isEmpty)
      assert(!option.isDefined)
      assert(option.getOrElse("fallback") == "fallback")
      assertThrows[NoSuchElementException](option.get)
    }

    test("some rejects null payloads") {
      assertThrows[NullPointerException](OptionVal.some(null))
    }
  }
}
