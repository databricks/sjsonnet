package sjsonnet

import java.util.Arrays

/**
 * [[ValScope]]s which model the lexical scopes within
 * a Jsonnet file that bind variable names to [[Val]]s, as well as other
 * contextual information like `self` `this` or `super`.
 *
 * Note that scopes are standalone, and nested scopes are done by copying
 * and updating the array of bindings rather than using a linked list. This
 * is because the bindings array is typically pretty small and the constant
 * factor overhead from a cleverer data structure dominates any algorithmic
 * improvements
 *
 * The bindings array is private and only copy-on-write, so for nested scopes
 * which do not change it (e.g. those just updating `dollar0` or `self0`) the
 * bindings array can be shared cheaply.
 */
final class ValScope private (val bindings: Array[Lazy]) extends AnyVal {

  def length: Int = bindings.length

  def extend(newBindingsF: Array[(Val.Obj, Val.Obj) => Lazy] = null,
             newSelf: Val.Obj, newSuper: Val.Obj) = {
    val by = if(newBindingsF == null) 2 else newBindingsF.length + 2
    val b = Arrays.copyOf(bindings, bindings.length + by)
    b(bindings.length) = newSelf
    b(bindings.length+1) = newSuper
    if(newBindingsF != null) {
      var i = 0
      var j = bindings.length+2
      while(i < newBindingsF.length) {
        b(j) = newBindingsF(i).apply(newSelf, newSuper)
        i += 1
        j += 1
      }
      b
    }
    new ValScope(b)
  }

  def extendSimple(newBindingsV: Array[_ <: Lazy]) = {
    if(newBindingsV == null || newBindingsV.length == 0) this
    else {
      val b = Arrays.copyOf(bindings, bindings.length + newBindingsV.length)
      System.arraycopy(newBindingsV, 0, b, bindings.length, newBindingsV.length)
      new ValScope(b)
    }
  }

  def extendBy(num: Int) =
    if(num == 0) this
    else new ValScope(Arrays.copyOf(bindings, bindings.length + num))

  def extendSimple(l1: Lazy) = {
    val b = Arrays.copyOf(bindings, bindings.length+1)
    b(bindings.length) = l1
    new ValScope(b)
  }

  def extendSimple(l1: Lazy, l2: Lazy) = {
    val b = Arrays.copyOf(bindings, bindings.length+2)
    b(bindings.length) = l1
    b(bindings.length+1) = l2
    new ValScope(b)
  }

  def extendSimple(l1: Lazy, l2: Lazy, l3: Lazy) = {
    val b = Arrays.copyOf(bindings, bindings.length+3)
    b(bindings.length) = l1
    b(bindings.length+1) = l2
    b(bindings.length+2) = l3
    new ValScope(b)
  }
}

object ValScope{
  private[this] val emptyArr = new Array[Lazy](0)
  def empty = new ValScope(emptyArr)
}
