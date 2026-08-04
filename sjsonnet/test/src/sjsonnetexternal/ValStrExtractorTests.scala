package sjsonnetexternal

import sjsonnet.{Position, Val}
import utest._

object ValStrExtractorTests extends TestSuite {
  def tests: Tests = Tests {
    test("Val.Str can be unapplied outside the sjsonnet package") {
      val pos = new Position(null, 7)
      val value: Val = Val.Str(pos, "hello")

      val result = value match {
        case Val.Str(extractedPos, str) => Some((extractedPos, str))
        case _                          => None
      }

      assert(result.contains((pos, "hello")))
    }
  }
}
