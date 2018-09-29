package sjsonnet

import ammonite.ops.Path
import sjsonnet.Expr.Member.Visibility

object Lazy{
  def apply(calc0: => Val) = new Lazy(calc0)
}
class Lazy(calc0: => Val){
  lazy val force = calc0
}
sealed trait Val{
  def prettyName: String
}
object Val{
  def bool(b: Boolean) = if (b) True else False
  case object True extends Val{
    def prettyName = "boolean"
  }
  case object False extends Val{
    def prettyName = "boolean"
  }
  case object Null extends Val{
    def prettyName = "null"
  }
  case class Str(value: String) extends Val{
    def prettyName = "string"
  }
  case class Num(value: Double) extends Val{
    def prettyName = "number"
  }
  case class Arr(value: Seq[Lazy]) extends Val{
    def prettyName = "array"
  }
  object Obj{

    case class Member(add: Boolean, visibility: Visibility, invoke: (Obj, Option[Obj]) => Lazy)
  }
  case class Obj(value0: Map[String, Obj.Member],
                 triggerAsserts: Val.Obj => Unit,
                 `super`: Option[Obj]) extends Val{
    def prettyName = "object"

    def getVisibleKeys() = {
      def rec(current: Val.Obj): Seq[(String, Visibility)] = {
        current.`super`.toSeq.flatMap(rec) ++
        current.value0.map{case (k, m) => (k, m.visibility)}.toSeq
      }

      val mapping = collection.mutable.LinkedHashMap.empty[String, Boolean]
      for ((k, sep) <- rec(this)){
        (mapping.get(k), sep) match{
          case (None, Visibility.Hidden) => mapping(k) = true
          case (None, _)    => mapping(k) = false

          case (Some(false), Visibility.Hidden) => mapping(k) = true
          case (Some(true), Visibility.Unhide) => mapping(k) = false
          case (Some(x), _) => mapping(k) = x
        }
      }
      mapping
    }
    val valueCache = collection.mutable.Map.empty[Any, Lazy]
    def value(k: String, fileName: Path, offset: Int,  self: Obj = this) = {

      valueCache.getOrElseUpdate(
        // It is very rare that self != this, so fast-path the common case
        // where they are the same by avoiding tuple construction and hashing
        if(self == this) k else (k, self),
        valueRaw(k, self).getOrElse(Evaluator.fail("Field does not exist: " + k, fileName, offset))
      )

    }

    def mergeMember(l: Val, r: Val) = (l, r) match{
      case (Val.Str(l), Val.Str(r)) => Val.Str(l + r)
      case (Val.Num(l), Val.Num(r)) => Val.Num(l + r)
      case (l: Val.Obj, r: Val.Obj) => Evaluator.mergeObjects(l, r)
    }

    def valueRaw(k: String, self: Obj): Option[Lazy] = this.value0.get(k) match{
      case Some(m) =>
        def localResult = m.invoke(self, this.`super`).force
        this.`super` match{
          case Some(s) if m.add =>
            Some(Lazy(s.valueRaw(k, self).fold(localResult)(x => mergeMember(x.force, localResult))))
          case _ => Some(Lazy(localResult))
        }

      case None => this.`super`.flatMap(_.valueRaw(k, self))
    }
  }

  case class Func(length: Int,
                  value: Seq[(Option[String], Lazy)] => Val) extends Val{
    def prettyName = "function"
  }
}
