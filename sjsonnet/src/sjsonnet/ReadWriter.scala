package sjsonnet

/**
  * Typeclasses for easy conversion between [[Val]]s and Scala data types
  */
sealed abstract class ReadWriter[T] {
  def apply(t: Val): T
  def write(pos: Position, t: T): Val
}
object ReadWriter{
  implicit def OptionReadWriter[T](implicit r: ReadWriter[T]): ReadWriter[Option[T]] = new ReadWriter[Option[T]] {
    def apply(t: Val): Option[T] = t match {
      case Val.Null(_) => None
      case other => Some(r(other))
    }

    def write(pos: Position, t: Option[T]): Val = t match {
      case None => Val.Null(pos)
      case Some(v) => r.write(pos, v)
    }
  }
  implicit object StringRead extends ReadWriter[String]{
    def apply(t: Val): String = t.asString
    def write(pos: Position, t: String): sjsonnet.Val.Str = Val.Str(pos, t)
  }
  implicit object BooleanRead extends ReadWriter[Boolean]{
    def apply(t: Val): Boolean = t.asBoolean
    def write(pos: Position, t: Boolean): sjsonnet.Val.Bool = Val.bool(pos, t)
  }
  implicit object IntRead extends ReadWriter[Int]{
    def apply(t: Val): Int = t.asInt
    def write(pos: Position, t: Int): sjsonnet.Val.Num = Val.Num(pos, t.toDouble)
  }
  implicit object LongRead extends ReadWriter[Long]{
    def apply(t: Val): Long = t.asLong
    def write(pos: Position, t: Long): sjsonnet.Val.Num= Val.Num(pos, t.toDouble)
  }
  implicit object DoubleRead extends ReadWriter[Double]{
    def apply(t: Val): Double = t.asDouble
    def write(pos: Position, t: Double): sjsonnet.Val.Num= Val.Num(pos, t)
  }
  implicit object ValRead extends ReadWriter[Val]{
    def apply(t: Val): Val = t
    def write(pos: Position, t: Val): Val = t
  }
  implicit object ObjRead extends ReadWriter[Val.Obj]{
    def apply(t: Val): Val.Obj = t.asObj
    def write(pos: Position, t: Val.Obj): sjsonnet.Val.Obj= t
  }
  implicit object ArrRead extends ReadWriter[Val.Arr]{
    def apply(t: Val): Val.Arr = t.asArr
    def write(pos: Position, t: Val.Arr): sjsonnet.Val.Arr= t
  }
  implicit object FuncRead extends ReadWriter[Val.Func]{
    def apply(t: Val): Val.Func = t.asFunc
    def write(pos: Position, t: Val.Func): sjsonnet.Val.Func= t
  }
  implicit object BuiltinRead  extends ReadWriter[Val.Builtin] {
    def apply(t: Val): Val.Builtin = t.asInstanceOf[Val.Builtin]
    def write(pos: Position, t: Val.Builtin): sjsonnet.Val.Builtin= t
  }
  implicit object LiteralRead extends ReadWriter[Val.Literal] {
    def apply(t: Val): Val.Literal = t match {
      case literal: Val.Literal => literal
      case other => Error.fail("Wrong parameter type: expected Val.Literal" + ", got " + other.prettyName)
    }

    def write(pos: Position, t: Val.Literal): sjsonnet.Val.Literal = t
  }
}
