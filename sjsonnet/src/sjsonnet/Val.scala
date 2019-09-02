package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Lazy{
  def apply(calc0: => Val) = new Lazy(calc0)
}
class Lazy(calc0: => Val){
  lazy val force = calc0
}
sealed trait Val{
  def prettyName: String
  def cast[T: ClassTag: PrettyNamed] =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(this)) this.asInstanceOf[T]
    else throw new DelegateError(
      "Expected " + implicitly[PrettyNamed[T]].s + ", found " + prettyName
    )
}
class PrettyNamed[T](val s: String)
object PrettyNamed{
  implicit def strName: PrettyNamed[Val.Str] = new PrettyNamed("string")
  implicit def numName: PrettyNamed[Val.Num] = new PrettyNamed("number")
  implicit def arrName: PrettyNamed[Val.Arr] = new PrettyNamed("array")
  implicit def objName: PrettyNamed[Val.Obj] = new PrettyNamed("object")
  implicit def funName: PrettyNamed[Val.Func] = new PrettyNamed("function")
}
object Val{
  def bool(b: Boolean) = if (b) True else False
  sealed trait Bool extends Val{
  }
  case object True extends Bool{
    def prettyName = "boolean"
  }
  case object False extends Bool{
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
                      invoke: (Obj, Option[Obj], ScopeApi) => Val,
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
    val valueCache = collection.mutable.Map.empty[Any, Val]
    def value(k: String,
              scope: ScopeApi,
              offset: Int,
              evaluator: EvaluatorApi,
              self: Obj = this): Val = {

      val cacheKey = if(self eq this) k else (k, self)

      val cacheLookuped =
        if (value0.get(k).exists(_.cached == false)) None
        else valueCache.get(cacheKey)

      cacheLookuped match{
        case Some(res) => res
        case None =>
          valueRaw(k, self, scope, evaluator, offset) match{
            case Some(x) =>
              valueCache(cacheKey) = x
              x
            case None =>
              Evaluator.fail("Field does not exist: " + k, scope.currentFile, offset, evaluator.wd)
          }
      }
    }

    def mergeMember(l: Val,
                    r: Val,
                    evaluator: EvaluatorApi,
                    currentFile: Path,
                    offset: Int) = (l, r) match{
      case (Val.Str(l), Val.Str(r)) => Val.Str(l + r)
      case (Val.Num(l), Val.Num(r)) => Val.Num(l + r)
      case (Val.Arr(l), Val.Arr(r)) => Val.Arr(l ++ r)
      case (l: Val.Obj, r: Val.Obj) => Evaluator.mergeObjects(l, r)
      case (Val.Str(l), r) =>
        try Val.Str(l + Materializer.apply(r, evaluator).transform(new Renderer()).toString)
        catch Evaluator.tryCatch2(currentFile, evaluator.wd, offset)
      case (l, Val.Str(r)) =>
        try Val.Str(Materializer.apply(l, evaluator).transform(new Renderer()).toString + r)
        catch Evaluator.tryCatch2(currentFile, evaluator.wd, offset)
    }

    @tailrec final def valueCached(k: String): Option[Boolean] = this.value0.get(k) match{
      case Some(m) => Some(m.cached)

      case None => this.`super` match{
        case None => None
        case Some(s) => s.valueCached(k)
      }
    }

    def valueRaw(k: String,
                 self: Obj,
                 scope: ScopeApi,
                 evaluator: EvaluatorApi,
                 offset: Int): Option[Val] = this.value0.get(k) match{
      case Some(m) =>
        this.`super` match{
          case Some(s) if m.add =>
            Some(
              s.valueRaw(k, self, scope, evaluator, offset) match{
                case None => m.invoke(self, this.`super`, scope)
                case Some(x) =>
                  mergeMember(
                    x,
                    m.invoke(self, this.`super`, scope),
                    evaluator,
                    scope.currentFile,
                    offset
                  )
              }
            )
          case _ =>
            Some(m.invoke(self, this.`super`, scope))
        }

      case None => this.`super` match{
        case None => None
        case Some(s) => s.valueRaw(k, self, scope, evaluator, offset)
      }
    }
  }

  case class Func(scope: Option[Scope],
                  params: Params,
                  evalRhs: (Scope, String, EvaluatorApi, Int) => Val,
                  evalDefault: (Expr, Scope) => Val = null) extends Val{
    def prettyName = "function"
    def apply(args: Seq[(Option[String], Lazy)],
              thisFile: String,
              evaluator: EvaluatorApi,
              outerOffset: Int,
              callerPath: Path) = {

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
          Evaluator.fail(
            "Too many args, function has " + params.args.length + " parameter(s)",
            callerPath,
            outerOffset,
            evaluator.wd
          )
      }
      lazy val seen = collection.mutable.Set.empty[String]
      for((k, v) <- newScope2){
        if (seen(k)) Evaluator.fail("Parameter passed more than once: " + k, callerPath, outerOffset, evaluator.wd)
        else seen.add(k)
      }

      lazy val missing = params.args.collect{case (s, None) => s}.toSet -- seen
      if (missing.nonEmpty){
        Evaluator.fail(
          s"Function parameter${if (missing.size > 1) "s" else ""} ${missing.mkString(", ")} not bound in call" ,
          callerPath, outerOffset, evaluator.wd
        )
      }

      lazy val unexpectedParams = seen -- params.args.collect{case (s, _) => s}.toSet
      if (unexpectedParams.nonEmpty) {
        Evaluator.fail(
          s"Function has no parameter${if (missing.size > 1) "s" else ""} ${unexpectedParams.mkString(", ")}",
          callerPath, outerOffset, evaluator.wd
        )
      }

      lazy val newScope: Scope  = scope match{
        case None => Scope.empty(evaluator.wd) ++ newScope1 ++ newScope2
        case Some(s) => s ++ newScope1 ++ newScope2
      }
      evalRhs(newScope, thisFile, evaluator, outerOffset)
    }
  }
}
