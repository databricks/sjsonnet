package sjsonnet

/*-
 * Changed:
 * - 80f58d4e2d5e4d4ea94ef828962ef5d8cba1a625: implements support for safe select operator ?.
 */

import java.util

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Expr.Params

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * [[Lazy]] models lazy evaluation within a Jsonnet program. Lazily
 * evaluated dictionary values, array contents, or function parameters
 * are all wrapped in [[Lazy]] and only truly evaluated on-demand
 */
abstract class Lazy {
  protected[this] var cached: Val = null
  def compute(): Val
  final def force: Val = {
    if(cached == null)  cached = compute()
    cached
  }
}

/**
  * [[Val]]s represented Jsonnet values that are the result of evaluating
  * a Jsonnet program. The [[Val]] data structure is essentially a JSON tree,
  * except evaluation of object attributes and array contents are lazy, and
  * the tree can contain functions.
  */
sealed abstract class Val extends Lazy {
  cached = this // avoid a megamorphic call to compute() when forcing
  final def compute() = this

  def pos: Position
  def prettyName: String

  def cast[T: ClassTag: PrettyNamed] =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(this)) this.asInstanceOf[T]
    else Error.fail("Expected " + implicitly[PrettyNamed[T]].s + ", found " + prettyName)

  private[this] def failAs(err: String): Nothing =
    Error.fail("Wrong parameter type: expected " + err + ", got " + prettyName)

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

  class Arr(val pos: Position, private val value: Array[_ <: Lazy]) extends Literal {
    def prettyName = "array"
    override def asArr: Arr = this
    def length: Int = value.length
    def force(i: Int) = value(i).force

    def asLazy(i: Int) = value(i)
    def asLazyArray: Array[Lazy] = value.asInstanceOf[Array[Lazy]]
    def asStrictArray: Array[Val] = value.map(_.force)

    def concat(newPos: Position, rhs: Arr): Arr =
      new Arr(newPos, value ++ rhs.value)

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

    abstract class Member(val add: Boolean, val visibility: Visibility, val cached: Boolean = true) {
      def invoke(self: Obj, sup: Obj, fs: FileScope, ev: EvalScope): Val
    }

    class ConstMember(add: Boolean, visibility: Visibility, v: Val, cached: Boolean = true)
      extends Member(add, visibility, cached) {
      def invoke(self: Obj, sup: Obj, fs: FileScope, ev: EvalScope): Val = v
    }

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
    var asserting: Boolean = false

    def getSuper = `super`

    private[this] def getValue0: util.LinkedHashMap[String, Obj.Member] = {
      if(value0 == null) {
        val value0 = new java.util.LinkedHashMap[String, Val.Obj.Member]
        allKeys.forEach { (k, _) =>
          value0.put(k, new Val.Obj.ConstMember(false, Visibility.Normal, valueCache(k)))
        }
        // Only assign to field after initialization is complete to allow unsynchronized multi-threaded use:
        this.value0 = value0
      }
      value0
    }

    @tailrec def triggerAllAsserts(obj: Val.Obj): Unit = {
      if(triggerAsserts != null) triggerAsserts(obj)
      if(`super` != null) `super`.triggerAllAsserts(obj)
    }

    def addSuper(pos: Position, lhs: Val.Obj): Val.Obj = {
      `super` match{
        case null => new Val.Obj(pos, getValue0, false, triggerAsserts, lhs)
        case x => new Val.Obj(pos, getValue0, false, triggerAsserts, x.addSuper(pos, lhs))
      }
    }

    def prettyName = "object"
    override def asObj: Val.Obj = this

    private def gatherKeys(mapping: util.LinkedHashMap[String, java.lang.Boolean]): Unit = {
      if(static) mapping.putAll(allKeys)
      else {
        if(`super` != null) `super`.gatherKeys(mapping)
        getValue0.forEach { (k, m) =>
          val vis = m.visibility
          if(!mapping.containsKey(k)) mapping.put(k, vis == Visibility.Hidden)
          else if(vis == Visibility.Hidden) mapping.put(k, true)
          else if(vis == Visibility.Unhide) mapping.put(k, false)
        }
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
              self: Obj = this,
              safe: Boolean = false)
             (implicit evaluator: EvalScope): Val = {
      if(static) {
        valueCache.getOrElse(k, null) match {
          case null => if (safe) Val.Null(pos) else Error.fail("Field does not exist: " + k, pos)
          case x => x
        }
      } else {
        val cacheKey = if(self eq this) k else (k, self)
        valueCache.getOrElse(cacheKey, {
          valueRaw(k, self, pos, valueCache, cacheKey) match {
            case null => if (safe) Val.Null(pos) else Error.fail("Field does not exist: " + k, pos)
            case x => x
          }
        })
      }
    }

    private def renderString(v: Val)(implicit evaluator: EvalScope): String =
      evaluator.materialize(v).transform(new Renderer()).toString

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
      if(static) {
        val v = valueCache.getOrElse(k, null)
        if(addTo != null && v != null) addTo(addKey) = v
        v
      } else {
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

    def foreachElement(sort: Boolean, pos: Position)(f: (String, Val) => Unit)(implicit ev: EvalScope): Unit = {
      val keys = if (sort) visibleKeyNames.sorted else visibleKeyNames
      for(k <- keys) {
        val v = value(k, pos)
        f(k, v)
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
                      val params: Params) extends Val with Expr {

    def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val

    def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val = null

    def prettyName = "function"

    override def exprErrorString: String = "Function"

    override def asFunc: Func = this

    def apply(argsL: Array[_ <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val = {
      val simple = namedNames == null && params.names.length == argsL.length
      val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
      //println(s"apply: argsL: ${argsL.length}, namedNames: $namedNames, paramNames: ${params.names.mkString(",")}")
      if(simple) {
        val newScope = defSiteValScope.extendSimple(argsL)
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      } else {
        val newScopeLen = math.max(params.names.length, argsL.length)
        // Initialize positional args
        val base = defSiteValScope.length
        val newScope = defSiteValScope.extendBy(newScopeLen)
        val argVals = newScope.bindings
        val posArgs = if(namedNames == null) argsL.length else argsL.length - namedNames.length
        System.arraycopy(argsL, 0, argVals, base, posArgs)
        if(namedNames != null) { // Add named args
          var i = 0
          var j = posArgs
          while(i < namedNames.length) {
            val idx = params.paramMap.getOrElse(namedNames(i),
              Error.fail(s"Function has no parameter ${namedNames(i)}", outerPos))
            if(argVals(base+idx) != null)
              Error.fail(s"binding parameter a second time: ${namedNames(i)}", outerPos)
            argVals(base+idx) = argsL(j)
            i += 1
            j += 1
          }
        }
        if(argsL.length > params.names.length)
          Error.fail("Too many args, function has " + params.names.length + " parameter(s)", outerPos)
        if(params.names.length != argsL.length) { // Args missing -> add defaults
          var missing: ArrayBuffer[String] = null
          var i = posArgs
          var j = base + posArgs
          while(j < argVals.length) {
            if(argVals(j) == null) {
              val default = params.defaultExprs(i)
              if(default != null) {
                argVals(j) = () => evalDefault(default, newScope, ev)
              } else {
                if(missing == null) missing = new ArrayBuffer
                missing.+=(params.names(i))
              }
            }
            i += 1
            j += 1
          }
          if(missing != null) {
            val plural = if(missing.size > 1) "s" else ""
            Error.fail(s"Function parameter$plural ${missing.mkString(", ")} not bound in call", outerPos)
          }
          //println(argVals.mkString(s"params: ${params.names.length}, argsL: ${argsL.length}, argVals: [", ", ", "]"))
        }
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }

    def apply0(outerPos: Position)(implicit ev: EvalScope): Val = {
      if(params.names.length != 0) apply(Evaluator.emptyLazyArray, null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
        evalRhs(defSiteValScope, ev, funDefFileScope, outerPos)
      }
    }

    def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope): Val = {
      if(params.names.length != 1) apply(Array(argVal), null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal)
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }

    def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val = {
      if(params.names.length != 2) apply(Array(argVal1, argVal2), null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal1, argVal2)
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }

    def apply3(argVal1: Lazy, argVal2: Lazy, argVal3: Lazy, outerPos: Position)(implicit ev: EvalScope): Val = {
      if(params.names.length != 3) apply(Array(argVal1, argVal2, argVal3), null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal1, argVal2, argVal3)
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }
  }

  /** Superclass for standard library functions */
  abstract class Builtin(paramNames: Array[String], defaults: Array[Expr] = null)
    extends Func(null, ValScope.empty, Params(paramNames,
      if(defaults == null) new Array[Expr](paramNames.length) else defaults)) {

    override final def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val = expr.asInstanceOf[Val]

    final def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val = {
      val args = new Array[Val](params.names.length)
      var i = 0
      var j = scope.length - args.length
      while(i < args.length) {
        args(i) = scope.bindings(i).force
        i += 1
      }
      evalRhs(args, ev, pos)
    }

    def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val

    override def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.names.length != 1) apply(Array(argVal), null, outerPos)
      else evalRhs(Array(argVal.force), ev, outerPos)

    override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.names.length != 2) apply(Array(argVal1, argVal2), null, outerPos)
      else evalRhs(Array(argVal1.force, argVal2.force), ev, outerPos)

    /** Specialize a call to this function in the optimizer. Must return either `null` to leave the
     * call-site as it is or a pair of a (possibly different) `Builtin` and the arguments to pass
     * to it (usually a subset of the supplied arguments).
     * @param args the positional arguments for this function call. Named arguments and defaults have
     *             already been resolved. */
    def specialize(args: Array[Expr]): (Builtin, Array[Expr]) = null

    /** Is this builtin safe to use in static evaluation */
    def staticSafe: Boolean = true
  }

  abstract class Builtin1(pn1: String, def1: Expr = null) extends Builtin(Array(pn1), if(def1 == null) null else Array(def1)) {
    final def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0), ev, pos)

    def evalRhs(arg1: Val, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[_ <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val =
      if(namedNames == null && argVals.length == 1) evalRhs(argVals(0).force, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)

    override def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.names.length == 1) evalRhs(argVal.force, ev, outerPos)
      else super.apply(Array(argVal), null, outerPos)
  }

  abstract class Builtin2(pn1: String, pn2: String, defs: Array[Expr] = null) extends Builtin(Array(pn1, pn2), defs) {
    final def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0), args(1), ev, pos)

    def evalRhs(arg1: Val, arg2: Val, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[_ <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val =
      if(namedNames == null && argVals.length == 2)
        evalRhs(argVals(0).force, argVals(1).force, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)

    override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.names.length == 2) evalRhs(argVal1.force, argVal2.force, ev, outerPos)
      else super.apply(Array(argVal1, argVal2), null, outerPos)
  }

  abstract class Builtin3(pn1: String, pn2: String, pn3: String, defs: Array[Expr] = null) extends Builtin(Array(pn1, pn2, pn3), defs) {
    final def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0), args(1), args(2), ev, pos)

    def evalRhs(arg1: Val, arg2: Val, arg3: Val, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[_ <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val =
      if(namedNames == null && argVals.length == 3)
        evalRhs(argVals(0).force, argVals(1).force, argVals(2).force, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)
  }

  abstract class Builtin4(pn1: String, pn2: String, pn3: String, pn4: String, defs: Array[Expr] = null) extends Builtin(Array(pn1, pn2, pn3, pn4), defs) {
    final def evalRhs(args: Array[Val], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0), args(1), args(2), args(3), ev, pos)

    def evalRhs(arg1: Val, arg2: Val, arg3: Val, arg4: Val, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[_ <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val =
      if(namedNames == null && argVals.length == 4)
        evalRhs(argVals(0).force, argVals(1).force, argVals(2).force, argVals(3).force, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)
  }
}

/**
  * [[EvalScope]] models the per-evaluator context that is propagated
  * throughout the Jsonnet evaluation.
  */
abstract class EvalScope extends EvalErrorScope {
  def visitExpr(expr: Expr)
               (implicit scope: ValScope): Val

  def materialize(v: Val): ujson.Value

  def equal(x: Val, y: Val): Boolean

  val emptyMaterializeFileScope = new FileScope(wd / "(materialize)")
  val emptyMaterializeFileScopePos = new Position(emptyMaterializeFileScope, -1)

  def settings: Settings
  def warn(e: Error): Unit
}
