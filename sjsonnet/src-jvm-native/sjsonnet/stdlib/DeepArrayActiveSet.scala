package sjsonnet.stdlib

import sjsonnet.Val

private[stdlib] final class DeepArrayActiveSet(initialCapacity: Int) {
  private val active =
    new java.util.IdentityHashMap[Val.Arr, java.lang.Boolean](initialCapacity)

  @inline def contains(arr: Val.Arr): Boolean =
    active.containsKey(arr)

  @inline def add(arr: Val.Arr): Boolean =
    active.put(arr, java.lang.Boolean.TRUE) eq null

  @inline def remove(arr: Val.Arr): Unit =
    active.remove(arr)
}
