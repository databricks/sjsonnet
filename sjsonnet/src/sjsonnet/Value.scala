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
  case class Obj(value0: Map[String, (Boolean, Obj => Ref)],
                 `super`: Option[Obj]) extends Value{
    val valueCache = collection.mutable.Map.empty[(String, Obj), Ref]
    def value(k: String, self: Obj = this) = valueCache.getOrElseUpdate(
      (k, self),
      {
        def rec(current: Obj, acc: List[Ref]): Ref = {
          current.value0.get(k) match{
            case Some((add, ref)) =>
              if (!add) {
                Ref(acc.iterator.map(_.calc).foldLeft(ref(self).calc){
                  case (Value.Str(l), Value.Str(r)) => Value.Str(l + r)
                  case (Value.Num(l), Value.Num(r)) => Value.Num(l + r)
                })
              }
              else {
                current.`super` match{
                  case None => Ref(acc.iterator.map(_.calc).foldLeft(ref(self).calc){
                    case (Value.Str(l), Value.Str(r)) => Value.Str(l + r)
                    case (Value.Num(l), Value.Num(r)) => Value.Num(l + r)
                  })
                  case Some(s) => rec(s, ref(self) :: acc)
                }
              }
            case None => current.`super` match{
              case None => ???
              case Some(s) => rec(s, acc)
            }
          }
        }
        rec(this, Nil)
      }
    )
  }
  case class Func(value: Seq[(Option[String], Ref)] => Value) extends Value
}
