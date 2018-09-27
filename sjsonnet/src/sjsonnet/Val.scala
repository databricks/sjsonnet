package sjsonnet

object Ref{
  def apply(calc0: => Val) = new Ref(calc0)
}
class Ref(calc0: => Val){
  lazy val calc = calc0
  def force(self: Option[Val.Obj]): Val = calc
}
sealed trait Val
object Val{
  def bool(b: Boolean) = if (b) True else False
  case object True extends Val
  case object False extends Val
  case object Null extends Val
  case class Str(value: String) extends Val
  case class Num(value: Double) extends Val
  case class Arr(value: Seq[Ref]) extends Val
  case class Obj(value0: Map[String, (Boolean, String, (Obj, Option[Obj]) => Ref)],
                 `super`: Option[Obj]) extends Val{

    def getVisibleKeys() = {
      def rec(current: Val.Obj): Seq[(String, String)] = {
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
                  case (Val.Str(l), Val.Str(r)) => Val.Str(l + r)
                  case (Val.Num(l), Val.Num(r)) => Val.Num(l + r)
                  case (l: Val.Obj, r: Val.Obj) => Evaluator.mergeObjects(l, r)
                })
              }
              else {
                current.`super` match{
                  case None => Ref(acc.iterator.map(_.calc).foldLeft(ref(self, current.`super`).calc){
                    case (Val.Str(l), Val.Str(r)) => Val.Str(l + r)
                    case (Val.Num(l), Val.Num(r)) => Val.Num(l + r)
                    case (l: Val.Obj, r: Val.Obj) => Evaluator.mergeObjects(l, r)
                  })
                  case Some(s) => rec(s, ref(self, current.`super`) :: acc)
                }
              }
            case None => current.`super` match{
              case None =>
                if (acc.isEmpty) throw new Exception("Unknown key: " + k)
                else Ref(acc.iterator.map(_.calc).reduceLeft[Val]{
                  case (Val.Str(l), Val.Str(r)) => Val.Str(l + r)
                  case (Val.Num(l), Val.Num(r)) => Val.Num(l + r)
                  case (l: Val.Obj, r: Val.Obj) => Evaluator.mergeObjects(l, r)
                })
              case Some(s) => rec(s, acc)
            }
          }
        }
        rec(this, Nil)
      }
    )
  }
  case class Func(length: Int, value: Seq[(Option[String], Ref)] => Val) extends Val
}
