package sjsonnet

import java.util

/**
 * [[ValScope]]s which model the lexical scopes within a Jsonnet file that bind variable names to
 * [[Val]]s, as well as other contextual information like `self` `this` or `super`.
 *
 * Note that scopes are standalone, and nested scopes are done by copying and updating the array of
 * bindings rather than using a linked list. This is because the bindings array is typically pretty
 * small and the constant factor overhead from a cleverer data structure dominates any algorithmic
 * improvements
 *
 * The bindings array is private and only copy-on-write, so for nested scopes which do not change it
 * (e.g. those just updating `dollar0` or `self0`) the bindings array can be shared cheaply.
 */
final class ValScope private (val bindings: Array[Lazy]) extends AnyVal {

  def length: Int = bindings.length

  def extend(newBindings: Array[Lazy], newSelf: Val.Obj, newSuper: Val.Obj): ValScope = {
    val b = util.Arrays.copyOf(bindings, bindings.length + newBindings.length + 2)
    b(bindings.length) = newSelf
    b(bindings.length + 1) = newSuper
    System.arraycopy(newBindings, 0, b, bindings.length + 2, newBindings.length)
    new ValScope(b)
  }

  def extendSimple(newBindingsV: Array[? <: Lazy]): ValScope = {
    if (newBindingsV == null || newBindingsV.length == 0) this
    else {
      val b = util.Arrays.copyOf(bindings, bindings.length + newBindingsV.length)
      System.arraycopy(newBindingsV, 0, b, bindings.length, newBindingsV.length)
      new ValScope(b)
    }
  }

  def extendBy(num: Int): ValScope =
    if (num == 0) this
    else new ValScope(util.Arrays.copyOf(bindings, bindings.length + num))

  def extendSimple(l1: Lazy): ValScope = {
    val b = util.Arrays.copyOf(bindings, bindings.length + 1)
    b(bindings.length) = l1
    new ValScope(b)
  }

  def extendSimple(l1: Lazy, l2: Lazy): ValScope = {
    val b = util.Arrays.copyOf(bindings, bindings.length + 2)
    b(bindings.length) = l1
    b(bindings.length + 1) = l2
    new ValScope(b)
  }

  def extendSimple(l1: Lazy, l2: Lazy, l3: Lazy): ValScope = {
    val b = util.Arrays.copyOf(bindings, bindings.length + 3)
    b(bindings.length) = l1
    b(bindings.length + 1) = l2
    b(bindings.length + 2) = l3
    new ValScope(b)
  }
}

object ValScope {
  private val emptyArr = new Array[Lazy](0)
  def empty = new ValScope(emptyArr)
}
