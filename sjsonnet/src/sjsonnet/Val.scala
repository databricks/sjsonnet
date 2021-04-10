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
sealed abstract class Val {
  def pos: Position
  def prettyName: String

  def cast[T: ClassTag: PrettyNamed] =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(this)) this.asInstanceOf[T]
    else throw new Error.Delegate(
      "Expected " + implicitly[PrettyNamed[T]].s + ", found " + prettyName
    )

  private[this] def failAs(err: String): Nothing =
    throw new Error.Delegate("Wrong parameter type: expected " + err + ", got " + prettyName)

  def asString: String = failAs("String")
  def asBoolean: Boolean = failAs("Boolean")
  def asInt: Int = failAs("Int")
  def asDouble: Double = failAs("Number")
  def asObj: Val.Obj = failAs("Object")
  def asArr: Val.Arr = failAs("Array")
  def asFunc: Val.Func = failAs("Function")
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
    final def force: Val = {
      if(cached == null) cached = compute()
      cached
    }
  }

  abstract class Literal extends Val with Expr
  abstract class Bool extends Literal {
    override def asBoolean: Boolean = this.isInstanceOf[True]
  }

  def bool(pos: Position, b: Boolean) = if (b) True(pos) else False(pos)

  case class True(pos: Position) extends Bool {
    def prettyName = "boolean"
  }
  case class False(pos: Position) extends Bool {
    def prettyName = "boolean"
  }
  case class Null(pos: Position) extends Literal {
    def prettyName = "null"
  }
  case class Str(pos: Position, value: String) extends Literal {
    def prettyName = "string"
    override def asString: String = value
  }
  case class Num(pos: Position, value: Double) extends Literal {
    def prettyName = "number"
    override def asInt: Int = value.toInt
    override def asDouble: Double = value
  }

  class Arr(val pos: Position, private val value: Array[Lazy]) extends Val{
    def prettyName = "array"
    override def asArr: Arr = this
    def length: Int = value.length
    def force(i: Int) = value(i).force

    def asLazy(i: Int) = value(i)
    def asLazyArray: Array[Lazy] = value
    def asStrictArray: Array[Val] = value.map(_.force)

    def concat(newPos: Position, rhs: Arr): Arr =
      new Arr(newPos, value ++ rhs.value)

    def slice(start: Int, end: Int, stride: Int): Arr = {
      val range = start until end by stride
      new Arr(pos, range.dropWhile(_ < 0).takeWhile(_ < value.length).map(value).toArray)
    }

    def iterator: Iterator[Val] = value.iterator.map(_.force)
    def foreach[U](f: Val => U) = {
      var i = 0
      while(i < value.length) {
        f(value(i).force)
        i += 1
      }
    }
    def forall(f: Val => Boolean): Boolean = {
      var i = 0
      while(i < value.length) {
        if(!f(value(i).force)) return false
        i += 1
      }
      true
    }
  }

  object Obj{

    case class Member(add: Boolean,
                      visibility: Visibility,
                      invoke: (Obj, Obj, FileScope, EvalScope) => Val,
                      cached: Boolean = true)

    def mk(pos: Position, members: (String, Obj.Member)*): Obj = {
      val m = new util.LinkedHashMap[String, Obj.Member]()
      for((k, v) <- members) m.put(k, v)
      new Obj(pos, m, false, null, null)
    }
  }

  final class Obj(val pos: Position,
                  private[this] var value0: util.LinkedHashMap[String, Obj.Member],
                  static: Boolean,
                  triggerAsserts: Val.Obj => Unit,
                  `super`: Obj,
                  valueCache: mutable.HashMap[Any, Val] = mutable.HashMap.empty[Any, Val],
                  private[this] var allKeys: util.LinkedHashMap[String, java.lang.Boolean] = null) extends Literal with Expr.ObjBody {

    def getSuper = `super`

    private[this] def getValue0: util.LinkedHashMap[String, Obj.Member] = {
      if(value0 == null) {
        value0 = new java.util.LinkedHashMap[String, Val.Obj.Member]
        allKeys.forEach { (k, _) =>
          val v = valueCache(k)
          val m = Val.Obj.Member(false, Visibility.Normal, (self: Val.Obj, sup: Val.Obj, _, _) => v)
          value0.put(k, m)
        }
      }
      value0
    }

    @tailrec def triggerAllAsserts(obj: Val.Obj): Unit = {
      if(triggerAsserts != null) triggerAsserts(obj)
      if(`super` != null) `super`.triggerAllAsserts(obj)
    }

    def addSuper(pos: Position, lhs: Val.Obj): Val.Obj = {
      `super` match{
        case null => new Val.Obj(pos, getValue0, false, null, lhs)
        case x => new Val.Obj(pos, getValue0, false, null, x.addSuper(pos, lhs))
      }
    }

    def prettyName = "object"
    override def asObj: Val.Obj = this

    private def gatherKeys(mapping: util.LinkedHashMap[String, java.lang.Boolean]): Unit = {
      if(`super` != null) `super`.gatherKeys(mapping)
      getValue0.forEach { (k, m) =>
        val vis = m.visibility
        if(!mapping.containsKey(k)) mapping.put(k, vis == Visibility.Hidden)
        else if(vis == Visibility.Hidden) mapping.put(k, true)
        else if(vis == Visibility.Unhide) mapping.put(k, false)
      }
    }

    private def getAllKeys = {
      if(allKeys == null) {
        allKeys = new util.LinkedHashMap[String, java.lang.Boolean]
        gatherKeys(allKeys)
      }
      allKeys
    }

    @inline def hasKeys = !getAllKeys.isEmpty

    @inline def containsKey(k: String): Boolean = getAllKeys.containsKey(k)

    @inline def containsVisibleKey(k: String): Boolean = getAllKeys.get(k) == java.lang.Boolean.FALSE

    lazy val allKeyNames: Array[String] = getAllKeys.keySet().toArray(new Array[String](getAllKeys.size()))

    lazy val visibleKeyNames: Array[String] = if(static) allKeyNames else {
      val buf = mutable.ArrayBuilder.make[String]
      getAllKeys.forEach((k, b) => if(b == java.lang.Boolean.FALSE) buf += k)
      buf.result()
    }

    def value(k: String,
              pos: Position,
              self: Obj = this)
             (implicit evaluator: EvalScope): Val = {

      val cacheKey = if(self eq this) k else (k, self)

      valueCache.getOrElse(cacheKey, {
        valueRaw(k, self, pos, valueCache, cacheKey) match {
          case null => Error.fail("Field does not exist: " + k, pos)
          case x => x
        }
      })
    }

    private def renderString(v: Val)(implicit evaluator: EvalScope): String = {
      try evaluator.materialize(v).transform(new Renderer()).toString
      catch Error.tryCatchWrap(pos)
    }

    def mergeMember(l: Val,
                    r: Val,
                    pos: Position)
                   (implicit evaluator: EvalScope) = {
      val lStr = l.isInstanceOf[Val.Str]
      val rStr = r.isInstanceOf[Val.Str]
      if(lStr || rStr) {
        val ll = if(lStr) l.asInstanceOf[Val.Str].value else renderString(l)
        val rr = if(rStr) r.asInstanceOf[Val.Str].value else renderString(r)
        Val.Str(pos, ll ++ rr)
      } else if(l.isInstanceOf[Val.Num] && r.isInstanceOf[Val.Num]) {
        val ll = l.asInstanceOf[Val.Num].value
        val rr = r.asInstanceOf[Val.Num].value
        Val.Num(pos, ll + rr)
      } else if(l.isInstanceOf[Val.Arr] && r.isInstanceOf[Val.Arr]) {
        val ll = l.asInstanceOf[Val.Arr].asLazyArray
        val rr = r.asInstanceOf[Val.Arr].asLazyArray
        new Val.Arr(pos, ll ++ rr)
      } else if(l.isInstanceOf[Val.Obj] && r.isInstanceOf[Val.Obj]) {
        val ll = l.asInstanceOf[Val.Obj]
        val rr = r.asInstanceOf[Val.Obj]
        rr.addSuper(pos, ll)
      } else throw new MatchError((l, r))
    }

    def valueRaw(k: String,
                 self: Obj,
                 pos: Position,
                 addTo: mutable.HashMap[Any, Val] = null,
                 addKey: Any = null)
                (implicit evaluator: EvalScope): Val = {
      val s = this.`super`
      getValue0.get(k) match{
        case null =>
          if(s == null) null else s.valueRaw(k, self, pos, addTo, addKey)
        case m =>
          val vv = m.invoke(self, s, pos.fileScope, evaluator)
          val v = if(s != null && m.add) {
            s.valueRaw(k, self, pos, null, null) match {
              case null => vv
              case supValue => mergeMember(supValue, vv, pos)
            }
          } else vv
          if(addTo != null && m.cached) addTo(addKey) = v
          v
      }
    }
  }

  def staticObject(pos: Position, fields: Array[Expr.Member.Field]): Obj = {
    val cache = mutable.HashMap.empty[Any, Val]
    val allKeys = new util.LinkedHashMap[String, java.lang.Boolean]
    fields.foreach {
      case Expr.Member.Field(_, Expr.FieldName.Fixed(k), _, _, _, rhs: Val.Literal) =>
        cache.put(k, rhs)
        allKeys.put(k, false)
    }
    new Val.Obj(pos, null, true, null, null, cache, allKeys)
  }

  abstract class Func(val pos: Position,
                      val defSiteValScope: ValScope,
                      val params: Params) extends Val {

    def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val

    def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val = null

    def prettyName = "function"

    override def asFunc: Func = this

    def apply(argNames: Array[String], argVals: Array[Lazy], outerPos: Position)(implicit ev: EvalScope): Val = {
      val argsSize = argVals.length
      val simple = argNames == null && params.indices.length == argsSize
      val passedArgsBindingsI = if(argNames != null) {
        val arrI: Array[Int] = new Array(argsSize)
        var i = 0
        try {
          while (i < argsSize) {
            val aname = argNames(i)
            arrI(i) = if(aname != null) params.argIndices.getOrElse(
              aname,
              Error.fail(s"Function has no parameter $aname", outerPos)
            ) else params.indices(i)
            i += 1
          }
        } catch { case e: IndexOutOfBoundsException =>
          Error.fail("Too many args, function has " + params.names.length + " parameter(s)", outerPos)
        }
        arrI
      } else if(params.indices.length < argsSize) {
        Error.fail(
          "Too many args, function has " + params.names.length + " parameter(s)",
          outerPos
        )
      } else params.indices // Don't cut down to size to avoid copying. The correct size is argVals.length!

      val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }

      val newScope: ValScope = {
        if(simple) {
          defSiteValScope match {
            case null => ValScope.createSimple(argVals)
            case s => s.extendSimple(passedArgsBindingsI, argVals)
          }
        } else {
          val defaultArgsBindingIndices = params.defaultsOnlyIndices
          lazy val newScope: ValScope = {
            val defaultArgsBindings = new Array[Lazy](params.defaultsOnly.length)
            var idx = 0
            while (idx < params.defaultsOnly.length) {
              val default = params.defaultsOnly(idx)
              defaultArgsBindings(idx) = () => evalDefault(default, newScope, ev)
              idx += 1
            }
            defSiteValScope match {
              case null => ValScope.createSimple(defaultArgsBindingIndices, defaultArgsBindings, passedArgsBindingsI, argVals)
              case s => s.extendSimple(defaultArgsBindingIndices, defaultArgsBindings, passedArgsBindingsI, argVals)
            }
          }
          validateFunctionCall(passedArgsBindingsI, params, outerPos, funDefFileScope, argsSize)
          newScope
        }
      }

      evalRhs(newScope, ev, funDefFileScope, outerPos)
    }

    def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope): Val = {
      if(params.indices.length != 1) apply(null, Array(argVal), outerPos)
      else {
        val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
        val newScope: ValScope = defSiteValScope match {
          case null => ValScope.createSimple(argVal)
          case s => s.extendSimple(params.indices(0), argVal)
        }
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }

    def apply1(argVal: Val, outerPos: Position)(implicit ev: EvalScope): Val = apply1(() => argVal, outerPos)

    def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val = {
      if(params.indices.length != 2) apply(null, Array(argVal1, argVal2), outerPos)
      else {
        val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
        val newScope: ValScope = defSiteValScope match {
          case null => ValScope.createSimple(params.indices, Array(argVal1, argVal2))
          case s => s.extendSimple(params.indices, Array(argVal1, argVal2))
        }
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }

    def apply2(argVal1: Val, argVal2: Val, outerPos: Position)(implicit ev: EvalScope): Val =
      apply2(() => argVal1, () => argVal2, outerPos)

    def validateFunctionCall(passedArgsBindingsI: Array[Int],
                             params: Params,
                             outerPos: Position,
                             defSiteFileScope: FileScope,
                             argListSize: Int)
                            (implicit eval: EvalScope): Unit = {

      val seen = new util.BitSet(argListSize)
      var idx = 0
      while (idx < argListSize) {
        val i = passedArgsBindingsI(idx)
        seen.set(i)
        idx += 1
      }

      if(argListSize != params.names.length || argListSize != seen.cardinality()) {
        seen.clear()
        val repeats = new util.BitSet(argListSize)

        idx = 0
        while (idx < argListSize) {
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
          b,
          outerPos,
          (plural, names) => s"Function parameter$plural $names not bound in call",
          defSiteFileScope // pass the definition site for the correct error message/names to be resolved
        )
        seen.andNot(params.allIndices)
        Error.failIfNonEmpty(
          seen,
          outerPos,
          (plural, names) => s"Function has no parameter$plural $names",
          outerPos.fileScope
        )
      }
    }
  }

  abstract class Builtin(paramNames: String*)
    extends Func(null, null, Params(paramNames.toArray, new Array[Expr](paramNames.length), paramNames.indices.toArray)) {

    final def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val = {
      val args = new Array[Val](params.names.length)
      var i = 0
      while(i < args.length) {
        args(i) = scope.bindings(i).force
        i += 1
      }
      evalRhs(args, ev, pos)
    }

    def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val

    override def apply(argNames: Array[String], argVals: Array[Lazy],
              outerPos: Position)
             (implicit ev: EvalScope): Val = {

      val argsSize = argVals.length
      val simple = argNames == null && params.indices.length == argsSize
      val passedArgsBindingsI = if(argNames != null) {
        val arrI: Array[Int] = new Array(argsSize)
        var i = 0
        try {
          while (i < argsSize) {
            val aname = argNames(i)
            arrI(i) = if(aname != null) params.argIndices.getOrElse(
              aname,
              Error.fail(s"Function has no parameter $aname", outerPos)
            ) else params.indices(i)
            i += 1
          }
        } catch { case e: IndexOutOfBoundsException =>
          Error.fail("Too many args, function has " + params.names.length + " parameter(s)", outerPos)
        }
        arrI
      } else if(params.indices.length < argsSize) {
        Error.fail(
          "Too many args, function has " + params.names.length + " parameter(s)",
          outerPos
        )
      } else params.indices // Don't cut down to size to avoid copying. The correct size is argVals.length!

      val newScope = ValScope.createSimple(passedArgsBindingsI, argVals)
      if(!simple) validateFunctionCall(passedArgsBindingsI, params, outerPos, outerPos.fileScope, argsSize)

      evalRhs(newScope, ev, outerPos.fileScope, outerPos)
    }

    override def apply1(argVal: Val, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.indices.length != 1) apply(null, Array(() => argVal), outerPos)
      else evalRhs(Array(argVal), ev, outerPos)

    override def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.indices.length != 1) apply(null, Array(argVal), outerPos)
      else evalRhs(Array(argVal.force), ev, outerPos)

    override def apply2(argVal1: Val, argVal2: Val, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.indices.length != 2) apply(null, Array(() => argVal1, () => argVal2), outerPos)
      else evalRhs(Array(argVal1, argVal2), ev, outerPos)

    override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.indices.length != 2) apply(null, Array(argVal1, argVal2), outerPos)
      else evalRhs(Array(argVal1.force, argVal2.force), ev, outerPos)
  }

  abstract class Builtin1(pn1: String) extends Builtin(pn1) {
    final def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0), ev, pos)

    def evalRhs(arg1: Val, ev: EvalScope, pos: Position): Val

    override def apply(argNames: Array[String], argVals: Array[Lazy], outerPos: Position)(implicit ev: EvalScope): Val =
      if(argNames == null && argVals.length == 1) evalRhs(argVals(0).force, ev, outerPos)
      else super.apply(argNames, argVals, outerPos)

    override def apply1(argVal: Val, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.indices.length == 1) evalRhs(argVal, ev, outerPos)
      else super.apply(null, Array(() => argVal), outerPos)

    override def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.indices.length == 1) evalRhs(argVal.force, ev, outerPos)
      else super.apply(null, Array(argVal), outerPos)
  }

  abstract class Builtin2(pn1: String, pn2: String) extends Builtin(pn1, pn2) {
    final def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0), args(1), ev, pos)

    def evalRhs(arg1: Val, arg2: Val, ev: EvalScope, pos: Position): Val

    override def apply(argNames: Array[String], argVals: Array[Lazy], outerPos: Position)(implicit ev: EvalScope): Val =
      if(argNames == null && argVals.length == 2)
        evalRhs(argVals(0).force, argVals(1).force, ev, outerPos)
      else super.apply(argNames, argVals, outerPos)

    override def apply2(argVal1: Val, argVal2: Val, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.indices.length == 2) evalRhs(argVal1, argVal2, ev, outerPos)
      else super.apply(null, Array(() => argVal1, () => argVal2), outerPos)

    override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.indices.length == 2) evalRhs(argVal1.force, argVal2.force, ev, outerPos)
      else super.apply(null, Array(argVal1, argVal2), outerPos)
  }

  abstract class Builtin3(pn1: String, pn2: String, pn3: String) extends Builtin(pn1, pn2, pn3) {
    final def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0), args(1), args(2), ev, pos)

    def evalRhs(arg1: Val, arg2: Val, arg3: Val, ev: EvalScope, pos: Position): Val

    override def apply(argNames: Array[String], argVals: Array[Lazy], outerPos: Position)(implicit ev: EvalScope): Val =
      if(argNames == null && argVals.length == 3)
        evalRhs(argVals(0).force, argVals(1).force, argVals(2).force, ev, outerPos)
      else super.apply(argNames, argVals, outerPos)
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

  def equal(x: Val, y: Val): Boolean

  val emptyMaterializeFileScope = new FileScope(wd / "(materialize)", Map())
  val emptyMaterializeFileScopePos = new Position(emptyMaterializeFileScope, -1)

  val preserveOrder: Boolean = false
}

object ValScope{
  def empty(size: Int) = new ValScope(null, null, null, new Array(size))

  def createSimple(newBindingV: Val.Lazy) = {
    val arr = new Array[Val.Lazy](1)
    arr(0) = newBindingV
    new ValScope(null, null, null, arr)
  }

  def createSimple(newBindingsV: Array[Val.Lazy]) =
    new ValScope(null, null, null, newBindingsV)

  def createSimple(newBindingsI: Array[Int],
                   newBindingsV: Array[Val.Lazy]) = {
    val arr = new Array[Val.Lazy](newBindingsI.length)
    var i = 0
    while(i < newBindingsI.length) {
      arr(newBindingsI(i)) = newBindingsV(i)
      i += 1
    }
    new ValScope(null, null, null, arr)
  }

  def createSimple(newBindingsI1: Array[Int],
                   newBindingsV1: Array[Val.Lazy],
                   newBindingsI2: Array[Int],
                   newBindingsV2: Array[Val.Lazy]) = {
    val arr = new Array[Val.Lazy](newBindingsV1.length + newBindingsV2.length)
    var i = 0
    while(i < newBindingsV1.length) {
      arr(newBindingsI1(i)) = newBindingsV1(i)
      i += 1
    }
    i = 0
    while(i < newBindingsV2.length) {
      arr(newBindingsI2(i)) = newBindingsV2(i)
      i += 1
    }
    new ValScope(null, null, null, arr)
  }
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

  def bindings(k: Int): Val.Lazy = bindings0(k)

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
    else {
      val b = bindings0.clone()
      var i = 0
      while(i < newBindingsI.length) {
        b(newBindingsI(i)) = newBindingsV(i)
        i += 1
      }
      new ValScope(dollar0, self0, super0, b)
    }
  }

  def extendSimple(newBindingsI1: Array[Int],
                   newBindingsV1: Array[Val.Lazy],
                   newBindingsI2: Array[Int],
                   newBindingsV2: Array[Val.Lazy]) = {
    val b = bindings0.clone()
    var i = 0
    while(i < newBindingsV1.length) {
      b(newBindingsI1(i)) = newBindingsV1(i)
      i += 1
    }
    i = 0
    while(i < newBindingsV2.length) {
      b(newBindingsI2(i)) = newBindingsV2(i)
      i += 1
    }
    new ValScope(dollar0, self0, super0, b)
  }

  def extendSimple(newBindingI: Int,
                   newBindingV: Val.Lazy) = {
    val b = bindings0.clone()
    b(newBindingI) = newBindingV
    new ValScope(dollar0, self0, super0, b)
  }
}
