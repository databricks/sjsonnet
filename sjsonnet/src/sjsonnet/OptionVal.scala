package sjsonnet

/**
 * Optional reference value similar to `Option`, represented as a value class over a nullable
 * reference. Use this only in allocation-sensitive internal paths where the payload type is a
 * non-null reference value.
 */
private[sjsonnet] object OptionVal {
  @inline def some[A <: AnyRef](x: A): OptionVal[A] = {
    if (x eq null) throw new NullPointerException("OptionVal.some(null)")
    new OptionVal(x)
  }

  /**
   * Boxed singleton sentinel; returning a freshly constructed null value class can erase to null.
   */
  val None: OptionVal[Null] = new OptionVal[Null](null)
}

private[sjsonnet] final class OptionVal[+A <: AnyRef](val x: A) extends AnyVal {
  @inline def isEmpty: Boolean = x eq null
  @inline def isDefined: Boolean = !isEmpty

  @inline def get: A =
    if (isEmpty) throw new NoSuchElementException("OptionVal.None.get")
    else x

  @inline def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else x
}
