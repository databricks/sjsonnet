package sjsonnet

/**
  * Typeclasses for easy conversion between [[Val]]s and Scala data types
  */
sealed abstract class ReadWriter[T] {
  def apply(t: Val): T
  def write(pos: Position, t: T): Val
}
object ReadWriter{
  implicit object StringRead extends ReadWriter[String]{
    def apply(t: Val) = t.asString
    def write(pos: Position, t: String) = Val.Str(pos, t)
  }
  implicit object BooleanRead extends ReadWriter[Boolean]{
    def apply(t: Val) = t.asBoolean
    def write(pos: Position, t: Boolean) = Val.bool(pos, t)
  }
  implicit object IntRead extends ReadWriter[Int]{
    def apply(t: Val) = t.asInt
    def write(pos: Position, t: Int) = Val.Num(pos, t)
  }
  implicit object DoubleRead extends ReadWriter[Double]{
    def apply(t: Val) = t.asDouble
    def write(pos: Position, t: Double) = Val.Num(pos, t)
  }
  implicit object ValRead extends ReadWriter[Val]{
    def apply(t: Val) = t
    def write(pos: Position, t: Val) = t
  }
  implicit object ObjRead extends ReadWriter[Val.Obj]{
    def apply(t: Val) = t.asObj
    def write(pos: Position, t: Val.Obj) = t
  }
  implicit object ArrRead extends ReadWriter[Val.Arr]{
    def apply(t: Val) = t.asArr
    def write(pos: Position, t: Val.Arr) = t
  }
  implicit object FuncRead extends ReadWriter[Val.Func]{
    def apply(t: Val) = t.asFunc
    def write(pos: Position, t: Val.Func) = t
  }
}
