package sjsonnet
import ujson.Js
object Materializer {
  def apply(v: Value): Js = v match{
    case Value.True => Js.True
    case Value.False => Js.False
    case Value.Null => Js.Null
    case Value.Num(n) => Js.Num(n)
    case Value.Str(s) => Js.Str(s)
    case Value.Arr(xs) => Js.Arr.from(xs.map(x => apply(x.calc)))
    case obj: Value.Obj =>

      def rec(current: Value.Obj): Seq[(String, String)] = {
        current.`super`.toSeq.flatMap(rec) ++ current.value0.map{case (k, (add, sep, f)) => (k, sep)}.toSeq
      }

      val mapping = collection.mutable.LinkedHashMap.empty[String, Boolean]
      for ((k, sep) <- rec(obj)){
        (mapping.get(k), sep) match{
          case (None, "::") => mapping(k) = true
          case (None, _)    => mapping(k) = false

          case (Some(false), "::") => mapping(k) = true
          case (Some(true), ":::") => mapping(k) = false
          case (Some(x), _) => mapping(k) = x
        }
      }

      Js.Obj.from(
        for {
          (k, hidden) <- mapping
          if !hidden
        }yield k -> apply(obj.value(k).calc)
      )
  }
}
