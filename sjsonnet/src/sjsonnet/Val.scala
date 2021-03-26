package sjsonnet

import java.util

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * [[Val]]s represented Jsonnet values that are the result of evaluating
  * a Jsonnet program. The [[Val]] data structure is essentially a JSON tree,
  * except evaluation of object attributes and array contents are lazy, and
  * the tree can contain functions.
  */
sealed trait Val{
  def prettyName: String
  def cast[T: ClassTag: PrettyNamed] =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(this)) this.asInstanceOf[T]
    else throw new Error.Delegate(
      "Expected " + implicitly[PrettyNamed[T]].s + ", found " + prettyName
    )
  def pos: Position
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

  /**
    * [[Lazy]] models lazy evaluation within a Jsonnet program. Lazily
    * evaluated dictionary values, array contents, or function parameters
    * are all wrapped in [[Lazy]] and only truly evaluated on-demand
    */
  abstract class Lazy {
    private[this] var cached: Val = null
    def compute(): Val
    def force: Val = {
      if(cached == null) cached = compute()
      cached
    }
  }

  def bool(pos: Position, b: Boolean) = if (b) True(pos) else False(pos)
  sealed trait Bool extends Val{
  }
  case class True(pos: Position) extends Bool{
    def prettyName = "boolean"
  }
  case class False(pos: Position) extends Bool{
    def prettyName = "boolean"
  }
  case class Null(pos: Position) extends Val{
    def prettyName = "null"
  }
  case class Str(pos: Position, value: String) extends Val{
    def prettyName = "string"
  }
  case class Num(pos: Position, value: Double) extends Val{
    def prettyName = "number"
  }
  case class Arr(pos: Position, value: Array[Lazy]) extends Val{
    def prettyName = "array"
  }
  object Obj{

    case class Member(add: Boolean,
                      visibility: Visibility,
                      invoke: (Obj, Obj, FileScope, EvalScope) => Val,
                      cached: Boolean = true)


  }
  final class Obj(val pos: Position,
                  value0: mutable.Map[String, Obj.Member],
                  triggerAsserts: Val.Obj => Unit,
                  `super`: Obj) extends Val{

    def getSuper = `super`

    @tailrec def triggerAllAsserts(obj: Val.Obj): Unit = {
      triggerAsserts(obj)
      `super` match {
        case null =>
        case s => s.triggerAllAsserts(obj)
      }
    }

    def addSuper(pos: Position, lhs: Val.Obj): Val.Obj = {
      `super` match{
        case null => new Val.Obj(pos, value0, _ => (), lhs)
        case x => new Val.Obj(pos, value0, _ => (), x.addSuper(pos, lhs))
      }
    }

    def prettyName = "object"

    def foreachVisibleKey(output: (String, Visibility) => Unit): Unit = {
      if(`super` != null) `super`.foreachVisibleKey(output)
      for(t <- value0) output(t._1, t._2.visibility)
    }

    def getVisibleKeys() = {
      val mapping = mutable.LinkedHashMap.empty[String, Boolean]
      foreachVisibleKey{ (k, sep) =>
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
    private[this] val valueCache = collection.mutable.Map.empty[Any, Val]
    def value(k: String,
              pos: Position,
              self: Obj = this)
             (implicit evaluator: EvalScope): Val = {

      val cacheKey = if(self eq this) k else (k, self)

      valueCache.get(cacheKey) match{
        case Some(res) => res
        case None =>
          valueRaw(k, self, pos) match{
            case Some((x, cached)) =>
              if (cached) valueCache(cacheKey) = x
              x
            case None =>
              Error.fail("Field does not exist: " + k, pos)
          }
      }
    }

    def mergeMember(l: Val,
                    r: Val,
                    pos: Position)
                   (implicit evaluator: EvalScope) = (l, r) match{
      case (Val.Str(_, l), Val.Str(_, r)) => Val.Str(pos, l + r)
      case (Val.Num(_, l), Val.Num(_, r)) => Val.Num(pos, l + r)
      case (Val.Arr(_, l), Val.Arr(_, r)) => Val.Arr(pos, l ++ r)
      case (l: Val.Obj, r: Val.Obj) => r.addSuper(pos, l)
      case (Val.Str(_, l), r) =>
        try Val.Str(pos, l + evaluator.materialize(r).transform(new Renderer()).toString)
        catch Error.tryCatchWrap(pos)
      case (l, Val.Str(_, r)) =>
        try Val.Str(pos, evaluator.materialize(l).transform(new Renderer()).toString + r)
        catch Error.tryCatchWrap(pos)
    }

    @tailrec def valueCached(k: String): Option[Boolean] = this.value0.get(k) match{
      case Some(m) => Some(m.cached)

      case None => this.`super` match{
        case null => None
        case s => s.valueCached(k)
      }
    }

    def valueRaw(k: String,
                 self: Obj,
                 pos: Position)
                (implicit evaluator: EvalScope): Option[(Val, Boolean)] = {
      this.value0.get(k) match{
        case Some(m) =>
          this.`super` match{
            case s if s != null && m.add =>
              val merged = s.valueRaw(k, self, pos) match{
                case None => m.invoke(self, this.`super`, pos.fileScope, evaluator)
                case Some((supValue, supCached)) =>
                  mergeMember(supValue, m.invoke(self, this.`super`, pos.fileScope, evaluator), pos)
              }

              Some(merged -> m.cached)

            case _ =>
              Some(m.invoke(self, this.`super`, pos.fileScope, evaluator) -> m.cached)
          }

        case None => this.`super` match{
          case null => None
          case s => s.valueRaw(k, self, pos)
        }
      }
    }

    @tailrec def containsKey(k: String): Boolean = {
      this.value0.contains(k) || {
        this.`super` match {
          case null => false
          case s => s.containsKey(k)
        }
      }
    }
  }

  case class Func(pos: Position,
                  defSiteValScope: ValScope,
                  params: Params,
                  evalRhs: (ValScope, String, EvalScope, FileScope, Position) => Val,
                  evalDefault: (Expr, ValScope, EvalScope) => Val = null) extends Val{
    def prettyName = "function"
    def apply(argNames: Array[String], argVals: Array[Lazy],
              thisFile: String,
              outerPos: Position)
             (implicit evaluator: EvalScope) = {

      val defaultArgsBindingIndices = params.defaultsOnlyIndices
      lazy val defaultArgsBindings: Array[Lazy] = {
        var idx = 0
        val arr = new Array[Lazy](params.defaultsOnly.length)
        while (idx < params.defaultsOnly.length) {
          val default = params.defaultsOnly(idx)
          arr(idx) = () => evalDefault(default, newScope, evaluator)
          idx += 1
        }
        arr
      }

      lazy val (passedArgsBindingsI, newScope) = {
        val argsSize = argVals.length
        val (passedArgsBindingsI, passedArgsBindingsV) = if(argNames != null) {
          val arrI: Array[Int] = new Array(argsSize)
          val arrV: Array[Lazy] = new Array(argsSize)
          var i = 0
          try {
            while (i < argsSize) {
              val aname = argNames(i)
              arrI(i) = if(aname != null) {
                val argIndex = params.argIndices.getOrElse(
                  aname,
                  Error.fail(s"Function has no parameter $aname", outerPos)
                )
                argIndex
              } else params.indices(i)
              arrV(i) = argVals(i)
              i += 1
            }
          } catch { case e: IndexOutOfBoundsException =>
            Error.fail(
              "Too many args, function has " + params.names.length + " parameter(s)",
              outerPos
            )
          }
          (arrI, arrV)
        } else {
          if(params.indices.length < argsSize)
            Error.fail(
              "Too many args, function has " + params.names.length + " parameter(s)",
              outerPos
            )
          val arrV: Array[Lazy] = new Array(argsSize)
          var i = 0
          while (i < argsSize) {
            arrV(i) = argVals(i)
            i += 1
          }
          val arrI = if(params.indices.length == argsSize) params.indices else util.Arrays.copyOf(params.indices, argsSize)
          (arrI, arrV)
        }

        var max = -1
        val newBindingsI = new Array[Int](defaultArgsBindings.length + passedArgsBindingsV.length)
        val newBindingsV = new Array[Lazy](newBindingsI.length)
        var idx = 0

        var defaultBindingsIdx = 0
        while (defaultBindingsIdx < defaultArgsBindings.length) {
          val i = defaultArgsBindingIndices(defaultBindingsIdx)
          val v = defaultArgsBindings(defaultBindingsIdx)
          if (i > max) max = i
          newBindingsI(idx) = i
          newBindingsV(idx) = v
          idx += 1
          defaultBindingsIdx += 1
        }

        var passedArgsBindingsIdx = 0
        while(passedArgsBindingsIdx < passedArgsBindingsV.length) {
          val i = passedArgsBindingsI(passedArgsBindingsIdx)
          val v = passedArgsBindingsV(passedArgsBindingsIdx)
          if (i > max) max = i
          newBindingsI(idx) = i
          newBindingsV(idx) = v
          idx += 1
          passedArgsBindingsIdx += 1
        }

        val newScope = defSiteValScope match{
          case null => new ValScope(
            null,
            null,
            null,
            {
              val arr = new Array[Lazy](max + 1)
              var i = 0
              while(i < newBindingsI.length) {
                arr(newBindingsI(i)) = newBindingsV(i)
                i += 1
              }
              arr
            }
          )
          case s => s.extendSimple(newBindingsI, newBindingsV)
        }
        (passedArgsBindingsI, newScope)
      }

      val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
      validateFunctionCall(passedArgsBindingsI, params, outerPos, funDefFileScope)

      evalRhs(
        newScope,
        thisFile,
        evaluator,
        funDefFileScope,
        outerPos
      )
    }

    def validateFunctionCall(passedArgsBindingsI: Array[Int],
                             params: Params,
                             outerPos: Position,
                             defSiteFileScope: FileScope)
                            (implicit eval: EvalScope): Unit = {

      val argListSize = passedArgsBindingsI.length
      val seen = new util.BitSet(argListSize)
      val repeats = new util.BitSet(argListSize)

      var idx = 0
      while (idx < passedArgsBindingsI.length) {
        val i = passedArgsBindingsI(idx)
        if (!seen.get(i)) seen.set(i)
        else repeats.set(i)
        idx += 1
      }

      Error.failIfNonEmpty(
        repeats,
        outerPos,
        (plural, names) => s"binding parameter a second time: $names",
        defSiteFileScope
      )

      val b = params.noDefaultIndices.clone().asInstanceOf[util.BitSet]
      b.andNot(seen)
      Error.failIfNonEmpty(
        // params.noDefaultIndices.filter(!seen.get(_)),
        b,
        outerPos,
        (plural, names) => s"Function parameter$plural $names not bound in call",
        defSiteFileScope // pass the definition site for the correct error message/names to be resolved
      )

      seen.andNot(params.allIndices)

      Error.failIfNonEmpty(
        // seen.filter(!params.allIndices(_)),
        seen,
        outerPos,
        (plural, names) => s"Function has no parameter$plural $names",
        outerPos.fileScope
      )
    }
  }
}

/**
  * [[EvalScope]] models the per-evaluator context that is propagated
  * throughout the Jsonnet evaluation.
  */
trait EvalScope extends EvalErrorScope{
  def visitExpr(expr: Expr)
               (implicit scope: ValScope): Val

  def materialize(v: Val): ujson.Value

  val emptyMaterializeFileScope = new FileScope(wd / "(materialize)", Map())

  val preserveOrder: Boolean = false
}
object ValScope{
  def empty(size: Int) = new ValScope(null, null, null, new Array(size))
}

/**
  * [[ValScope]]s which model the lexical scopes within
  * a Jsonnet file that bind variable names to [[Val]]s, as well as other
  * contextual information like `self` `this` or `super`.
  *
  * Note that scopes are standalone, and nested scopes are done by copying
  * and updating the array of bindings rather than using a linked list. This
  * is because the bindings array is typically pretty small and the constant
  * factor overhead from a cleverer data structure dominates any algorithmic
  * improvements
  *
  * The bindings array is private and only copy-on-write, so for nested scopes
  * which do not change it (e.g. those just updating `dollar0` or `self0`) the
  * bindings array can be shared cheaply.
  */
class ValScope(val dollar0: Val.Obj,
               val self0: Val.Obj,
               val super0: Val.Obj,
               bindings0: Array[Val.Lazy]) {

  def bindings(k: Int): Option[Val.Lazy] = bindings0(k) match{
    case null => None
    case v => Some(v)
  }

  def extend(newBindingsI: Array[Expr.Bind] = null,
             newBindingsF: Array[(Val.Obj, Val.Obj) => Val.Lazy] = null,
             newDollar: Val.Obj = null,
             newSelf: Val.Obj = null,
             newSuper: Val.Obj = null) = {
    val dollar = if (newDollar != null) newDollar else dollar0
    val self = if (newSelf != null) newSelf else self0
    val sup = if (newSuper != null) newSuper else super0
    new ValScope(
      dollar,
      self,
      sup,
      if (newBindingsI == null || newBindingsI.length == 0) bindings0
      else{
        val b = bindings0.clone()
        var i = 0
        while(i < newBindingsI.length) {
          b(newBindingsI(i).name) = newBindingsF(i).apply(self, sup)
          i += 1
        }
        b
      }
    )
  }

  def extendSimple(newBindingsI: Array[Int],
                   newBindingsV: Array[Val.Lazy]) = {
    if(newBindingsI == null || newBindingsI.length == 0) this
    else new ValScope(
      dollar0,
      self0,
      super0,
      {
        val b = bindings0.clone()
        var i = 0
        while(i < newBindingsI.length) {
          b(newBindingsI(i)) = newBindingsV(i)
          i += 1
        }
        b
      }
    )
  }

  def extendSimple(newBindingI: Int,
                   newBindingV: Val.Lazy) = new ValScope(
    dollar0,
    self0,
    super0,
    {
      val b = bindings0.clone()
      b(newBindingI) = newBindingV
      b
    }
  )
}
