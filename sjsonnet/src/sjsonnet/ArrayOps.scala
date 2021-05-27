package sjsonnet

import scala.collection.mutable
import scala.reflect.ClassTag

/** Array operations which are missing in Scala 2.12 */
object ArrayOps {
  def distinctBy[A <: AnyRef : ClassTag, B](xs: Array[A])(f: A => B): Array[A] = {
    val seen = new mutable.HashSet[B]
    val b = new mutable.ArrayBuilder.ofRef[A]()
    var i = 0
    while(i < xs.length) {
      val x = xs(i)
      if(seen.add(f(x))) b.+=(x)
      i += 1
    }
    b.result()
  }

  def sortInPlaceBy[A <: AnyRef, B](xs: Array[A])(f: A => B)(implicit ord: Ordering[B]): xs.type = {
    java.util.Arrays.sort(xs, 0, xs.length, ord on f)
    xs
  }
}
