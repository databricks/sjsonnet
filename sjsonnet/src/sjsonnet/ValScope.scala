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
class ValScope(val dollar0: Val.Obj,
               val self0: Val.Obj,
               val super0: Val.Obj,
               val bindings: Array[Lazy]) {

  def length: Int = bindings.length

  def extend(newBindingsF: Array[(Val.Obj, Val.Obj) => Lazy] = null,
             newDollar: Val.Obj = null,
             newSelf: Val.Obj = null,
             newSuper: Val.Obj = null) = {
    val dollar = if (newDollar != null) newDollar else dollar0
    val self = if (newSelf != null) newSelf else self0
    val sup = if (newSuper != null) newSuper else super0
    new ValScope(
      dollar,
      self,
      sup,
      if (newBindingsF == null || newBindingsF.length == 0) bindings
      else {
        val b = Arrays.copyOf(bindings, bindings.length + newBindingsF.length)
        var i = 0
        var j = bindings.length
        while(i < newBindingsF.length) {
          b(j) = newBindingsF(i).apply(self, sup)
          i += 1
          j += 1
        }
        b
      }
    )
  }

  def extendBy(num: Int, newDollar: Val.Obj, newSelf: Val.Obj, newSuper: Val.Obj) = {
    val dollar = if (newDollar != null) newDollar else dollar0
    val self = if (newSelf != null) newSelf else self0
    val sup = if (newSuper != null) newSuper else super0
    new ValScope(dollar, self, sup, Arrays.copyOf(bindings, bindings.length + num))
  }

  def extendSimple(newBindingsV: Array[_ <: Lazy]) = {
    if(newBindingsV == null || newBindingsV.length == 0) this
    else {
      val b = Arrays.copyOf(bindings, bindings.length + newBindingsV.length)
      System.arraycopy(newBindingsV, 0, b, bindings.length, newBindingsV.length)
      new ValScope(dollar0, self0, super0, b)
    }
  }

  def extendBy(num: Int) =
    if(num == 0) this
    else new ValScope(dollar0, self0, super0, Arrays.copyOf(bindings, bindings.length + num))

  def extendSimple(l1: Lazy) = {
    val b = Arrays.copyOf(bindings, bindings.length+1)
    b(bindings.length) = l1
    new ValScope(dollar0, self0, super0, b)
  }

  def extendSimple(l1: Lazy, l2: Lazy) = {
    val b = Arrays.copyOf(bindings, bindings.length+2)
    b(bindings.length) = l1
    b(bindings.length+1) = l2
    new ValScope(dollar0, self0, super0, b)
  }

  def extendSimple(l1: Lazy, l2: Lazy, l3: Lazy) = {
    val b = Arrays.copyOf(bindings, bindings.length+3)
    b(bindings.length) = l1
    b(bindings.length+1) = l2
    b(bindings.length+2) = l3
    new ValScope(dollar0, self0, super0, b)
  }
}

object ValScope{
  private[this] val emptyArr = new Array[Lazy](0)
  def empty = new ValScope(null, null, null, emptyArr)

  def createSimple(newBindingV: Lazy) = {
    val arr = new Array[Lazy](1)
    arr(0) = newBindingV
    new ValScope(null, null, null, arr)
  }

  def createSimple(newBindingsV: Array[_ <: Lazy]) =
    new ValScope(null, null, null, newBindingsV.asInstanceOf[Array[Lazy]])

  def createSimple(len: Int) =
    new ValScope(null, null, null, new Array[Lazy](len))

  final val INVALID_IDX = -1
}
