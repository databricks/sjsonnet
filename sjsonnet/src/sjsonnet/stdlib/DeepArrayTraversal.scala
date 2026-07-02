package sjsonnet.stdlib

import sjsonnet._

import scala.collection.mutable

private[stdlib] object DeepArrayTraversal {
  private final class ArrFrame(
      val arr: Val.Arr,
      var index: Int,
      val len: Int,
      val direct: Array[Eval])

  private final class ArrFrameStack(initialCapacity: Int) {
    private var frames = new Array[ArrFrame](initialCapacity)
    private var depth = 0

    @inline def isEmpty: Boolean = depth == 0

    @inline def size: Int = depth

    @inline def peek(): ArrFrame =
      frames(depth - 1)

    @inline def containsArray(arr: Val.Arr): Boolean = {
      var i = depth - 1
      while (i >= 0) {
        if (frames(i).arr eq arr) return true
        i -= 1
      }
      false
    }

    @inline def addArraysTo(active: DeepArrayActiveSet): Unit = {
      var i = 0
      while (i < depth) {
        assert(active.add(frames(i).arr), "active deep array stack should not contain duplicates")
        i += 1
      }
    }

    @inline def push(frame: ArrFrame): Unit = {
      if (depth == frames.length) grow()
      frames(depth) = frame
      depth += 1
    }

    @inline def pop(): ArrFrame = {
      depth -= 1
      val frame = frames(depth)
      frames(depth) = null
      frame
    }

    private def grow(): Unit = {
      val old = frames
      val next = new Array[ArrFrame](old.length << 1)
      System.arraycopy(old, 0, next, 0, old.length)
      frames = next
    }
  }

  private final class TraversalState(maxDepth: Int) {
    private val stack = new ArrFrameStack(InitialDepthCapacity)
    private var active: DeepArrayActiveSet = null

    @inline def isEmpty: Boolean = stack.isEmpty

    @inline def peek(): ArrFrame = stack.peek()

    @inline def pushArray(arr: Val.Arr): Unit =
      pushArray(arr, 0, arr.length, arr.directBackingArray)

    @inline def pushArray(arr: Val.Arr, index: Int, len: Int, direct: Array[Eval]): Unit = {
      if (stack.size >= maxDepth) Error.fail(MaxStackFramesExceededMessage)
      val currentActive = active
      if (currentActive eq null) {
        if (stack.containsArray(arr)) Error.fail(MaxStackFramesExceededMessage)
      } else if (!currentActive.add(arr)) {
        Error.fail(MaxStackFramesExceededMessage)
      }
      stack.push(new ArrFrame(arr, index, len, direct))
      if ((currentActive eq null) && stack.size == ActiveSetThreshold) {
        val nextActive = new DeepArrayActiveSet(ActiveSetThreshold)
        stack.addArraysTo(nextActive)
        active = nextActive
      }
    }

    @inline def popFrame(): Unit = {
      val popped = stack.pop()
      if (active ne null) active.remove(popped.arr)
    }
  }

  private val InitialDepthCapacity = 4
  private val ActiveSetThreshold = 32
  private val MaxStackFramesExceededMessage = "max stack frames exceeded"

  def appendFlattened(root: Val.Arr, out: mutable.ArrayBuilder.ofRef[Eval], maxDepth: Int): Unit = {
    if (maxDepth <= 0) Error.fail(MaxStackFramesExceededMessage)
    val rootDirect = root.directBackingArray
    val rootLen = root.length
    var rootIndex = 0
    while (rootIndex < rootLen) {
      val child =
        if (rootDirect ne null) rootDirect(rootIndex).value else root.value(rootIndex)
      rootIndex += 1
      child match {
        case arr: Val.Arr =>
          appendFlattenedFromNested(root, rootIndex, rootLen, rootDirect, arr, out, maxDepth)
          return
        case leaf => out += leaf
      }
    }
  }

  private def appendFlattenedFromNested(
      root: Val.Arr,
      rootIndex: Int,
      rootLen: Int,
      rootDirect: Array[Eval],
      nested: Val.Arr,
      out: mutable.ArrayBuilder.ofRef[Eval],
      maxDepth: Int): Unit = {
    val state = new TraversalState(maxDepth)
    state.pushArray(root, rootIndex, rootLen, rootDirect)
    state.pushArray(nested)
    while (!state.isEmpty) {
      val frame = state.peek()
      if (frame.index < frame.len) {
        val child = {
          val d = frame.direct
          if (d ne null) d(frame.index).value else frame.arr.value(frame.index)
        }
        frame.index += 1
        child match {
          case arr: Val.Arr => state.pushArray(arr)
          case leaf         => out += leaf
        }
      } else {
        state.popFrame()
      }
    }
  }

  def appendDeepJoined(root: Val.Arr, out: StringBuilderWriter, maxDepth: Int): Unit = {
    if (maxDepth <= 0) Error.fail(MaxStackFramesExceededMessage)
    val rootDirect = root.directBackingArray
    val rootLen = root.length
    var rootIndex = 0
    while (rootIndex < rootLen) {
      val child =
        if (rootDirect ne null) rootDirect(rootIndex).value else root.value(rootIndex)
      rootIndex += 1
      child match {
        case arr: Val.Arr =>
          if (appendDeepJoinedRootNested(root, rootIndex, rootLen, rootDirect, arr, out, maxDepth))
            return
        case s: Val.Str => out.write(s.str)
        case v          =>
          Error.fail("std.deepJoin: expected string or array, got " + v.prettyName)
      }
    }
  }

  // Scans the first nested segment separately so flat root/first-child cases
  // keep the fast path allocation-free and only build traversal state when the
  // remaining suffix needs an explicit stack.
  private def appendDeepJoinedRootNested(
      root: Val.Arr,
      rootIndex: Int,
      rootLen: Int,
      rootDirect: Array[Eval],
      initial: Val.Arr,
      out: StringBuilderWriter,
      maxDepth: Int): Boolean = {
    if (maxDepth <= 1) Error.fail(MaxStackFramesExceededMessage)
    if (initial eq root) Error.fail(MaxStackFramesExceededMessage)

    val direct = initial.directBackingArray
    val len = initial.length
    var index = 0
    var nested: Val.Arr = null
    while (index < len && (nested eq null)) {
      val child =
        if (direct ne null) direct(index).value else initial.value(index)
      index += 1
      child match {
        case arr: Val.Arr => nested = arr
        case s: Val.Str   => out.write(s.str)
        case v            =>
          Error.fail("std.deepJoin: expected string or array, got " + v.prettyName)
      }
    }
    if (nested eq null) return false

    val state = new TraversalState(maxDepth)
    state.pushArray(root, rootIndex, rootLen, rootDirect)
    state.pushArray(initial, index, len, direct)
    appendDeepJoinedNested(state, nested, out)
    appendDeepJoinedRemaining(state, out)
    true
  }

  private def appendDeepJoinedRemaining(state: TraversalState, out: StringBuilderWriter): Unit = {
    while (!state.isEmpty) {
      val frame = state.peek()
      if (frame.index < frame.len) {
        val child = {
          val d = frame.direct
          if (d ne null) d(frame.index).value else frame.arr.value(frame.index)
        }
        frame.index += 1
        child match {
          case arr: Val.Arr => appendDeepJoinedNested(state, arr, out)
          case s: Val.Str   => out.write(s.str)
          case v            =>
            Error.fail("std.deepJoin: expected string or array, got " + v.prettyName)
        }
      } else {
        state.popFrame()
      }
    }
  }

  private def appendDeepJoinedNested(
      state: TraversalState,
      initial: Val.Arr,
      out: StringBuilderWriter): Unit = {
    var current = initial
    while (true) {
      state.pushArray(current)
      val frame = state.peek()
      var nested: Val.Arr = null
      while (frame.index < frame.len && (nested eq null)) {
        val child =
          if (frame.direct ne null) frame.direct(frame.index).value
          else frame.arr.value(frame.index)
        frame.index += 1
        child match {
          case arr: Val.Arr => nested = arr
          case s: Val.Str   => out.write(s.str)
          case v            =>
            Error.fail("std.deepJoin: expected string or array, got " + v.prettyName)
        }
      }
      if (nested eq null) {
        state.popFrame()
        return
      }
      current = nested
    }
  }
}
