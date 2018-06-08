package sjsonnet

sealed trait Value
object Value{
  case object True extends Value
  case object False extends Value
  case object Null extends Value
  case class Str(value: String) extends Value
  case class Num(value: Double) extends Value
  case class Arr(value: Seq[Value]) extends Value
  case class Obj(value: Map[String, Value]) extends Value
}
