package sjsonnet

object CurrentPos {
  val currentPos: ThreadLocal[Position] = new ThreadLocal()
}
