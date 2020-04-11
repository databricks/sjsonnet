package object sjsonnet {
  // Compatibility layer until Scala Native supports Scala 2.12
  implicit class EitherOps[A,B](val e: Either[A,B]) extends AnyVal {
    @inline def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = e.right.flatMap(f)
    @inline def map[B1](f: B => B1): Either[A, B1] = e.right.map(f)
  }
}
