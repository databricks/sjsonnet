package sjsonnet

object Ref{
  def apply(calc0: => Value) = new Ref(calc0)
}
class Ref(calc0: => Value){
  lazy val calc = calc0
  def force(self: Option[Value.Obj]): Value = calc
}
sealed trait Value
object Value{
  case object True extends Value
  case object False extends Value
  case object Null extends Value
  case class Str(value: String) extends Value
  case class Num(value: Double) extends Value
  case class Arr(value: Seq[Ref]) extends Value
  case class Obj(value0: Map[String, (Boolean, Obj => Ref)]) extends Value{
    val valueCache = collection.mutable.Map.empty[String, Ref]
    def value(k: String) = valueCache.getOrElseUpdate(k, value0(k)._2(this))
  }
  case class Func(value: Seq[(Option[String], Ref)] => Value) extends Value
}
