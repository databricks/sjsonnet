package sjsonnet.stdlib

import sjsonnet.Val

private[stdlib] final class DeepArrayActiveSet(initialCapacity: Int) {
  private var keys = new Array[Val.Arr](tableSizeFor(initialCapacity))
  private var size = 0
  private var growAt = keys.length >> 1

  @inline def contains(arr: Val.Arr): Boolean = {
    val current = keys
    val mask = current.length - 1
    var slot = slotOf(arr, mask)
    while (true) {
      val key = current(slot)
      if (key eq null) return false
      if (key eq arr) return true
      slot = (slot + 1) & mask
    }
    false
  }

  @inline def add(arr: Val.Arr): Boolean = {
    val current = keys
    val mask = current.length - 1
    var slot = slotOf(arr, mask)
    while (true) {
      val key = current(slot)
      if (key eq null) {
        current(slot) = arr
        size += 1
        if (size >= growAt) grow()
        return true
      }
      if (key eq arr) return false
      slot = (slot + 1) & mask
    }
    false
  }

  @inline def remove(arr: Val.Arr): Unit = {
    val current = keys
    val mask = current.length - 1
    var slot = slotOf(arr, mask)
    while (true) {
      val key = current(slot)
      if (key eq null) return
      if (key eq arr) {
        removeAt(slot)
        return
      }
      slot = (slot + 1) & mask
    }
  }

  private def grow(): Unit = {
    val old = keys
    val next = new Array[Val.Arr](old.length << 1)
    keys = next
    growAt = next.length >> 1
    var i = 0
    while (i < old.length) {
      val key = old(i)
      if (key ne null) insertExisting(next, key)
      i += 1
    }
  }

  private def removeAt(slot0: Int): Unit = {
    val current = keys
    val mask = current.length - 1
    var slot = slot0
    var next = (slot + 1) & mask
    while (current(next) ne null) {
      val key = current(next)
      val home = slotOf(key, mask)
      if (((next - home) & mask) >= ((slot - home) & mask)) {
        current(slot) = key
        slot = next
      }
      next = (next + 1) & mask
    }
    current(slot) = null
    size -= 1
  }

  private def insertExisting(table: Array[Val.Arr], arr: Val.Arr): Unit = {
    val mask = table.length - 1
    var slot = slotOf(arr, mask)
    while (table(slot) ne null) {
      slot = (slot + 1) & mask
    }
    table(slot) = arr
  }

  @inline private def slotOf(arr: Val.Arr, mask: Int): Int = {
    var h = System.identityHashCode(arr)
    // Scala.js identityHashCode uses stable object ids; avalanche before
    // masking so regular id sequences do not cluster in power-of-two tables.
    h ^= h >>> 16
    h *= -2048144789
    h ^= h >>> 13
    h *= -1028477387
    h ^= h >>> 16
    h & mask
  }

  private def tableSizeFor(initialCapacity: Int): Int = {
    var n = 8
    val target = if (initialCapacity <= 0) 8 else initialCapacity << 1
    while (n < target) n <<= 1
    n
  }
}
