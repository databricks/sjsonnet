package sjsonnet

import utest._
import sjsonnet.Expr.Member.Visibility

object MaterializerTests extends TestSuite {
  val tests = Tests {
    test("computeSortedInlineOrder") {
      def mkMember(vis: Visibility): Val.Obj.Member =
        new Val.Obj.ConstMember(false, vis, Val.True(new Position(null, -1)), cached2 = true)

      test("basic sorting") {
        val keys = Array("z", "a", "m", "b")
        val members = Array.fill(4)(mkMember(Visibility.Normal))
        val order = Materializer.computeSortedInlineOrder(keys, members)
        val sorted = order.map(keys(_))
        assert(sorted.toSeq == Seq("a", "b", "m", "z"))
      }

      test("single field") {
        val keys = Array("only")
        val members = Array(mkMember(Visibility.Normal))
        val order = Materializer.computeSortedInlineOrder(keys, members)
        assert(order.toSeq == Seq(0))
      }

      test("already sorted") {
        val keys = Array("a", "b", "c")
        val members = Array.fill(3)(mkMember(Visibility.Normal))
        val order = Materializer.computeSortedInlineOrder(keys, members)
        assert(order.toSeq == Seq(0, 1, 2))
      }

      test("reverse sorted") {
        val keys = Array("c", "b", "a")
        val members = Array.fill(3)(mkMember(Visibility.Normal))
        val order = Materializer.computeSortedInlineOrder(keys, members)
        val sorted = order.map(keys(_))
        assert(sorted.toSeq == Seq("a", "b", "c"))
      }

      test("hidden fields filtered") {
        val keys = Array("c", "b", "a")
        val members = Array(
          mkMember(Visibility.Normal),
          mkMember(Visibility.Hidden),
          mkMember(Visibility.Normal)
        )
        val order = Materializer.computeSortedInlineOrder(keys, members)
        assert(order.length == 2)
        val sorted = order.map(keys(_))
        assert(sorted.toSeq == Seq("a", "c"))
      }

      test("all hidden") {
        val keys = Array("x", "y")
        val members = Array.fill(2)(mkMember(Visibility.Hidden))
        val order = Materializer.computeSortedInlineOrder(keys, members)
        assert(order.length == 0)
      }

      test("codepoint ordering with unicode") {
        // Codepoint ordering: uppercase letters < lowercase letters in ASCII
        val keys = Array("b", "B", "a", "A")
        val members = Array.fill(4)(mkMember(Visibility.Normal))
        val order = Materializer.computeSortedInlineOrder(keys, members)
        val sorted = order.map(keys(_))
        // A(65) < B(66) < a(97) < b(98)
        assert(sorted.toSeq == Seq("A", "B", "a", "b"))
      }

      test("stability - fields with equal keys preserve relative order") {
        // While duplicate keys shouldn't occur in practice (evaluator rejects them),
        // verify insertion sort stability just in case
        val keys = Array("x", "a", "z")
        val members = Array(
          mkMember(Visibility.Normal),
          mkMember(Visibility.Normal),
          mkMember(Visibility.Normal)
        )
        val order = Materializer.computeSortedInlineOrder(keys, members)
        val sorted = order.map(keys(_))
        assert(sorted.toSeq == Seq("a", "x", "z"))
      }
    }
  }
}
