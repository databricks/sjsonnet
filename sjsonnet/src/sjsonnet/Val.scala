package sjsonnet

import ammonite.ops.Path
import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params

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

    case class Member(add: Boolean,
                      visibility: Visibility,
                      invoke: (Obj, Option[Obj], String) => Lazy,
                      cached: Boolean = true)
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
    def value(k: String,
              fileName: Path,
              currentRoot: Path,
              offset: Int,
              wd: Path,
              extVars: Map[String, ujson.Js],
              self: Obj = this) = {

      val (cached, lazyValue) =
        valueRaw(k, self, fileName.relativeTo(currentRoot).toString(), extVars, wd, fileName, offset).getOrElse(Evaluator.fail("Field does not exist: " + k, fileName, offset, wd))
      if (!cached) lazyValue
      else valueCache.getOrElseUpdate(
        // It is very rare that self != this, so fast-path the common case
        // where they are the same by avoiding tuple construction and hashing
        if(self == this) k else (k, self),
        lazyValue
      )

    }

    def mergeMember(l: Val,
                    r: Val,
                    extVars: Map[String, ujson.Js],
                    wd: Path,
                    currentFile: Path,
                    offset: Int) = (l, r) match{
      case (Val.Str(l), Val.Str(r)) => Val.Str(l + r)
      case (Val.Num(l), Val.Num(r)) => Val.Num(l + r)
      case (Val.Arr(l), Val.Arr(r)) => Val.Arr(l ++ r)
      case (l: Val.Obj, r: Val.Obj) => Evaluator.mergeObjects(l, r)
      case (Val.Str(l), r) =>
        try Val.Str(l + Materializer.apply(r, extVars, wd).transform(new Renderer()).toString)
        catch Evaluator.tryCatch2(currentFile, wd, offset)
      case (l, Val.Str(r)) =>
        try Val.Str(Materializer.apply(l, extVars, wd).transform(new Renderer()).toString + r)
        catch Evaluator.tryCatch2(currentFile, wd, offset)
    }

    def valueRaw(k: String,
                 self: Obj,
                 thisFile: String,
                 extVars: Map[String, ujson.Js],
                 wd: Path,
                 currentFile: Path, offset: Int): Option[(Boolean, Lazy)] = this.value0.get(k) match{
      case Some(m) =>
        def localResult = m.invoke(self, this.`super`, thisFile).force
        this.`super` match{
          case Some(s) if m.add =>
            Some(m.cached -> Lazy(
              s.valueRaw(k, self, thisFile, extVars, wd, currentFile, offset).fold(localResult)(x =>
                mergeMember(x._2.force, localResult, extVars, wd, currentFile, offset)
              )
            ))
          case _ => Some(m.cached -> Lazy(localResult))
        }

      case None => this.`super`.flatMap(_.valueRaw(k, self, thisFile, extVars, wd, currentFile, offset))
    }
  }

  case class Func(scope: Scope,
                  params: Params,
                  evalRhs: (Scope, String, Map[String, ujson.Js], Int, Path) => Val,
                  evalDefault: (Expr, Scope) => Val = null) extends Val{
    def prettyName = "function"
    def apply(args: Seq[(Option[String], Lazy)],
              thisFile: String,
              extVars: Map[String, ujson.Js],
              outerOffset: Int,
              wd: Path) = {

      lazy val newScope1 =
        params.args.collect{
          case (k, Some(default)) => (k, (self: Val.Obj, sup: Option[Val.Obj]) => Lazy(evalDefault(default, newScope)))
        }

      lazy val newScope2 = try
        args.zipWithIndex.map{
          case ((Some(name), v), _) => (name, (self: Val.Obj, sup: Option[Val.Obj]) => v)
          case ((None, v), i) => (params.args(i)._1, (self: Val.Obj, sup: Option[Val.Obj]) => v)
        }
      catch{
        case e: IndexOutOfBoundsException =>
          Evaluator.fail("Too many args, function has " + params.args.length + " parameter(s)", scope.currentFile, outerOffset, wd)
      }
      lazy val seen = collection.mutable.Set.empty[String]
      for((k, v) <- newScope2){
        if (seen(k)) Evaluator.fail("Parameter passed more than once: " + k, scope.currentFile, outerOffset, wd)
        else seen.add(k)
      }

      lazy val newScope: Scope  = scope ++ newScope1 ++ newScope2
      evalRhs(newScope, thisFile, extVars, outerOffset, wd)
    }
  }
}
