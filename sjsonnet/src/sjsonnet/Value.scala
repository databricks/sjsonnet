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
  case class Obj(value0: Map[String, (Boolean, String, (Obj, Option[Obj]) => Ref)],
                 `super`: Option[Obj]) extends Value{

    def getVisibleKeys() = {
      def rec(current: Value.Obj): Seq[(String, String)] = {
        current.`super`.toSeq.flatMap(rec) ++ current.value0.map{case (k, (add, sep, f)) => (k, sep)}.toSeq
      }

      val mapping = collection.mutable.LinkedHashMap.empty[String, Boolean]
      for ((k, sep) <- rec(this)){
        (mapping.get(k), sep) match{
          case (None, "::") => mapping(k) = true
          case (None, _)    => mapping(k) = false

          case (Some(false), "::") => mapping(k) = true
          case (Some(true), ":::") => mapping(k) = false
          case (Some(x), _) => mapping(k) = x
        }
      }
      mapping
    }
    val valueCache = collection.mutable.Map.empty[(String, Obj), Ref]
    def value(k: String, self: Obj = this) = valueCache.getOrElseUpdate(
      (k, self),
      {
        def rec(current: Obj, acc: List[Ref]): Ref = {
          current.value0.get(k) match{
            case Some((add, _, ref)) =>
              if (!add) {
                Ref(acc.iterator.map(_.calc).foldLeft(ref(self, current.`super`).calc){
                  case (Value.Str(l), Value.Str(r)) => Value.Str(l + r)
                  case (Value.Num(l), Value.Num(r)) => Value.Num(l + r)
                })
              }
              else {
                current.`super` match{
                  case None => Ref(acc.iterator.map(_.calc).foldLeft(ref(self, current.`super`).calc){
                    case (Value.Str(l), Value.Str(r)) => Value.Str(l + r)
                    case (Value.Num(l), Value.Num(r)) => Value.Num(l + r)
                  })
                  case Some(s) => rec(s, ref(self, current.`super`) :: acc)
                }
              }
            case None => current.`super` match{
              case None => throw new Exception("Unknown key: " + k)
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
