package sjsonnet

import java.util
import java.util.Arrays
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
  protected var cached: Val = null
  def compute(): Val
  final def force: Val = {
    if(cached == null)  cached = compute()
    cached
  }
}

/**
 * Thread-safe implementation that discards the compute function after initialization.
 */
final class LazyWithComputeFunc(@volatile private var computeFunc: () => Val) extends Lazy {
  def compute(): Val = {
    val f = computeFunc
    if (f != null) {  // we won the race to initialize
      val result = f()
      cached = result
      computeFunc = null  // allow closure to be GC'd
    }
    // else: we lost the race to compute, but `cached` is already set and
    // is visible in this thread due to the volatile read and writes via
    // piggybacking; see https://stackoverflow.com/a/8769692 for background
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
  final def compute(): Val = this

  def pos: Position
  def prettyName: String

  def cast[T: ClassTag: PrettyNamed]: T =
    if (implicitly[ClassTag[T]].runtimeClass.isInstance(this)) this.asInstanceOf[T]
    else Error.fail("Expected " + implicitly[PrettyNamed[T]].s + ", found " + prettyName)

  private def failAs(err: String): Nothing =
    Error.fail("Wrong parameter type: expected " + err + ", got " + prettyName)

  def asString: String = failAs("String")
  def asBoolean: Boolean = failAs("Boolean")
  def asInt: Int = failAs("Int")
  def asLong: Long = failAs("Long")
  def asDouble: Double = failAs("Number")
  def asObj: Val.Obj = failAs("Object")
  def asArr: Val.Arr = failAs("Array")
  def asFunc: Val.Func = failAs("Function")
}

class PrettyNamed[T](val s: String)
object PrettyNamed{
  implicit val strName: PrettyNamed[Val.Str] = new PrettyNamed("string")
  implicit val numName: PrettyNamed[Val.Num] = new PrettyNamed("number")
  implicit val arrName: PrettyNamed[Val.Arr] = new PrettyNamed("array")
  implicit val boolName: PrettyNamed[Val.Bool] = new PrettyNamed("boolean")
  implicit val objName: PrettyNamed[Val.Obj] = new PrettyNamed("object")
  implicit val funName: PrettyNamed[Val.Func] = new PrettyNamed("function")
  implicit val nullName: PrettyNamed[Val.Null] = new PrettyNamed("null")
}
object Val{

  abstract class Literal extends Val with Expr {
    final override private[sjsonnet] val _tag = ExprTags.`Val.Literal`
  }
  abstract class Bool extends Literal {
    override def asBoolean: Boolean = this.isInstanceOf[True]
  }

  def bool(pos: Position, b: Boolean): Bool = if (b) True(pos) else False(pos)

  final case class True(pos: Position) extends Bool {
    def prettyName = "boolean"
  }
  final case class False(pos: Position) extends Bool {
    def prettyName = "boolean"
  }
  final case class Null(pos: Position) extends Literal {
    def prettyName = "null"
  }
  final case class Str(pos: Position, value: String) extends Literal {
    def prettyName = "string"
    override def asString: String = value
  }
  final case class Num(pos: Position, value: Double) extends Literal {
    def prettyName = "number"
    override def asInt: Int = value.toInt
    override def asLong: Long = value.toLong
    override def asDouble: Double = value
  }

  class Arr(val pos: Position, private val value: Array[? <: Lazy]) extends Literal {
    def prettyName = "array"

    override def asArr: Arr = this
    def length: Int = value.length
    def force(i: Int): Val = value(i).force

    def asLazyArray: Array[Lazy] = value.asInstanceOf[Array[Lazy]]
    def asStrictArray: Array[Val] = value.map(_.force)

    def concat(newPos: Position, rhs: Arr): Arr =
      new Arr(newPos, value ++ rhs.value)

    def iterator: Iterator[Val] = value.iterator.map(_.force)
    def foreach[U](f: Val => U): Unit = {
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

    /**
     * Helper for saving space in valueCache for objects without a super object.
     * For objects with no super, we (cheaply) know the exact number of fields and
     * therefore can upper bound the number of fields that _might_ be computed.
     */
    def getEmptyValueCacheForObjWithoutSuper(numFields: Int): util.HashMap[Any, Val] = {
      // We only want to pre-size if it yields a smaller initial map size than the default.
      if (numFields >= 12) {
        new util.HashMap[Any, Val]()
      } else {
        Util.preSizedJavaHashMap[Any, Val](numFields)
      }
    }

    /**
     * @param add whether this field was defined the "+:", "+::" or "+:::" separators, corresponding
     *            to the "nested field inheritance" language feature; see
     *            https://jsonnet.org/ref/language.html#nested-field-inheritance
     */
    abstract class Member(val add: Boolean, val visibility: Visibility, val cached: Boolean = true) {
      def invoke(self: Obj, sup: Obj, fs: FileScope, ev: EvalScope): Val
    }

    class ConstMember(add: Boolean, visibility: Visibility, v: Val, cached: Boolean = true)
      extends Member(add, visibility, cached) {
      def invoke(self: Obj, sup: Obj, fs: FileScope, ev: EvalScope): Val = v
    }

    def mk(pos: Position, members: (String, Obj.Member)*): Obj = {
      val m = Util.preSizedJavaLinkedHashMap[String, Obj.Member](members.length)
      for((k, v) <- members) m.put(k, v)
      new Obj(pos, m, false, null, null)
    }

    def mk(pos: Position, members: Array[(String, Obj.Member)]): Obj = {
      val m = Util.preSizedJavaLinkedHashMap[String, Obj.Member](members.length)
      var i = 0
      while (i < members.length) {
        val e = members(i)
        m.put(e._1, e._2)
        i += 1
      }
      new Obj(pos, m, false, null, null)
    }
  }

  /**
   * Represents json/jsonnet objects.
   *
   * Obj implements special optimizations for "static objects", which are objects without
   * `super` where all fields are constant and have default visibility. Static objects can
   * be created during parsing or in [[StaticOptimizer]].
   *
   * @param value0 maps fields to their Member definitions. This is initially null for
   *               static objects and is non-null for non-static objects.
   * @param static true if this object is static, false otherwise.
   * @param triggerAsserts callback to evaluate assertions defined in the object.
   * @param `super` the super object, or null if there is no super object.
   * @param valueCache a cache for computed values. For static objects, this is pre-populated
   *                   with all fields. For non-static objects, this is lazily populated as
   *                   fields are accessed.
   * @param allKeys a map of all keys in the object (including keys inherited from `super`),
   *                where the boolean value is true if the key is hidden and false otherwise.
   *                For static objects, this is pre-populated and the mapping may be interned
   *                and shared across instances. For non-static objects, it is dynamically
   *                computed only if the object has a `super`
   */
  final class Obj(val pos: Position,
                  private var value0: util.LinkedHashMap[String, Obj.Member],
                  static: Boolean,
                  triggerAsserts: Val.Obj => Unit,
                  `super`: Obj,
                  valueCache: util.HashMap[Any, Val] = new util.HashMap[Any, Val](),
                  private var allKeys: util.LinkedHashMap[String, java.lang.Boolean] = null) extends Literal with Expr.ObjBody {
    var asserting: Boolean = false

    def getSuper = `super`

    private def getValue0: util.LinkedHashMap[String, Obj.Member] = {
      // value0 is always defined for non-static objects, so if we're computing it here
      // then that implies that the object is static and therefore valueCache should be
      // pre-populated and all members should be visible and constant.
      if(value0 == null) {
        val value0 = Util.preSizedJavaLinkedHashMap[String, Val.Obj.Member](allKeys.size())
        allKeys.forEach { (k, _) =>
          value0.put(k, new Val.Obj.ConstMember(false, Visibility.Normal, valueCache.get(k)))
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
        val allKeys = new util.LinkedHashMap[String, java.lang.Boolean]
        gatherKeys(allKeys)
        // Only assign to field after initialization is complete to allow unsynchronized multi-threaded use:
        this.allKeys = allKeys
      }
      allKeys
    }

    @inline def hasKeys: Boolean = {
      val m = if (static || `super` != null) getAllKeys else value0
      !m.isEmpty
    }

    @inline def containsKey(k: String): Boolean = {
      val m = if (static || `super` != null) getAllKeys else value0
      m.containsKey(k)
    }

    @inline def containsVisibleKey(k: String): Boolean = {
      if (static || `super` != null) {
        getAllKeys.get(k) == java.lang.Boolean.FALSE
      } else {
        val m = value0.get(k)
        m != null && (m.visibility != Visibility.Hidden)
      }
    }

    lazy val allKeyNames: Array[String] = {
      val m = if (static || `super` != null) getAllKeys else value0
      m.keySet().toArray(new Array[String](m.size()))
    }

    lazy val visibleKeyNames: Array[String] = {
      if (static) {
        allKeyNames
      } else {
        val buf = new mutable.ArrayBuilder.ofRef[String]
        if (`super` == null) {
          // This size hint is based on an optimistic assumption that most fields are visible,
          // avoiding re-sizing or trimming the buffer in the common case:
          buf.sizeHint(value0.size())
          value0.forEach((k, m) => if (m.visibility != Visibility.Hidden) buf += k)
        } else {
          getAllKeys.forEach((k, b) => if (b == java.lang.Boolean.FALSE) buf += k)
        }
        buf.result()
      }
    }

    def value(k: String,
              pos: Position,
              self: Obj = this)
             (implicit evaluator: EvalScope): Val = {
      if(static) {
        valueCache.get(k) match {
          case null => Error.fail("Field does not exist: " + k, pos)
          case x => x
        }
      } else {
        val cacheKey = if(self eq this) k else (k, self)
        val cachedValue = valueCache.get(cacheKey)
        if (cachedValue != null) {
          cachedValue
        } else {
          valueRaw(k, self, pos, valueCache, cacheKey) match {
            case null => Error.fail("Field does not exist: " + k, pos)
            case x => x
          }
        }
      }
    }

    private def renderString(v: Val)(implicit evaluator: EvalScope): String =
      evaluator.materialize(v).transform(new Renderer()).toString

    /**
     * Merge two values for "nested field inheritance"; see
     * https://jsonnet.org/ref/language.html#nested-field-inheritance for background.
     */
    def mergeMember(l: Val,
                    r: Val,
                    pos: Position)
                   (implicit evaluator: EvalScope): Literal = {
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
                 addTo: util.HashMap[Any, Val] = null,
                 addKey: Any = null)
                (implicit evaluator: EvalScope): Val = {
      if(static) {
        val v = valueCache.get(k)
        if(addTo != null && v != null) addTo.put(addKey, v)
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
            if(addTo != null && m.cached) addTo.put(addKey, v)
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

    def staticSafe:Boolean = static
  }

  final class StaticObjectFieldSet(protected val keys: Array[String]) {

    override def hashCode(): Int = {
      Arrays.hashCode(keys.asInstanceOf[Array[Object]])
    }

    override def equals(obj: scala.Any): Boolean = {
      obj match {
        case that: StaticObjectFieldSet =>
          keys.sameElements(that.keys)
        case _ => false
      }
    }
  }

  def staticObject(
      pos: Position,
      fields: Array[Expr.Member.Field],
      internedKeyMaps: mutable.HashMap[StaticObjectFieldSet, java.util.LinkedHashMap[String, java.lang.Boolean]],
      internedStrings: mutable.HashMap[String, String]): Obj = {
    val cache = Util.preSizedJavaHashMap[Any, Val](fields.length)
    val allKeys = Util.preSizedJavaLinkedHashMap[String, java.lang.Boolean](fields.length)
    val keys = new Array[String](fields.length)
    var idx = 0
    fields.foreach {
      case Expr.Member.Field(_, Expr.FieldName.Fixed(k), _, _, _, rhs: Val.Literal) =>
        val uniqueKey = internedStrings.getOrElseUpdate(k, k)
        cache.put(uniqueKey, rhs)
        allKeys.put(uniqueKey, false)
        keys(idx) = uniqueKey
        idx += 1
      case other =>
        throw new Error(s"Unexpected non-literal field in static object: ${other} of class ${other.getClass}")
    }
    val fieldSet = new StaticObjectFieldSet(keys)
    new Val.Obj(pos, null, true, null, null, cache, internedKeyMaps.getOrElseUpdate(fieldSet, allKeys))
  }

  abstract class Func(val pos: Position,
                      val defSiteValScope: ValScope,
                      val params: Params) extends Val with Expr {
    final override private[sjsonnet] val _tag = ExprTags.`Val.Func`

    def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val

    def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val = null

    def prettyName = "function"

    override def exprErrorString: String = "Function"

    override def asFunc: Func = this

    def apply(argsL: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val = {
      val simple = namedNames == null && params.names.length == argsL.length
      val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
      //println(s"apply: argsL: ${argsL.length}, namedNames: $namedNames, paramNames: ${params.names.mkString(",")}")
      if (simple || ev.tailstrict) {
        if (ev.tailstrict) {
          argsL.foreach(_.force)
        }
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
                argVals(j) = new LazyWithComputeFunc(() => evalDefault(default, newScope, ev))
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
        if (ev.tailstrict) {
          argVal.force
        }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal)
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }

    def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val = {
      if(params.names.length != 2) apply(Array(argVal1, argVal2), null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
        if (ev.tailstrict) {
          argVal1.force
          argVal2.force
        }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal1, argVal2)
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }

    def apply3(argVal1: Lazy, argVal2: Lazy, argVal3: Lazy, outerPos: Position)(implicit ev: EvalScope): Val = {
      if(params.names.length != 3) apply(Array(argVal1, argVal2, argVal3), null, outerPos)
      else {
        val funDefFileScope: FileScope = pos match { case null => outerPos.fileScope case p => p.fileScope }
        if (ev.tailstrict) {
          argVal1.force
          argVal2.force
          argVal3.force
        }
        val newScope: ValScope = defSiteValScope.extendSimple(argVal1, argVal2, argVal3)
        evalRhs(newScope, ev, funDefFileScope, outerPos)
      }
    }
  }

  /** Superclass for standard library functions */
  abstract class Builtin(val functionName: String, paramNames: Array[String], defaults: Array[Expr] = null)
    extends Func(null, ValScope.empty, Params(paramNames,
      if(defaults == null) new Array[Expr](paramNames.length) else defaults)) {

    override final def evalDefault(expr: Expr, vs: ValScope, es: EvalScope): Val = expr.asInstanceOf[Val]

    override final def evalRhs(scope: ValScope, ev: EvalScope, fs: FileScope, pos: Position): Val = {
      evalRhs(scope.bindings, ev, pos)
    }

    def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val

    override def apply1(argVal: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.names.length != 1) apply(Array(argVal), null, outerPos)
      else {
        if (ev.tailstrict) {
          argVal.force
        }
        evalRhs(Array(argVal), ev, outerPos)
      }

    override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.names.length != 2) apply(Array(argVal1, argVal2), null, outerPos)
      else {
        if (ev.tailstrict) {
          argVal1.force
          argVal2.force
        }
        evalRhs(Array(argVal1, argVal2), ev, outerPos)
      }

    override def apply3(argVal1: Lazy, argVal2: Lazy, argVal3: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.names.length != 3) apply(Array(argVal1, argVal2, argVal3), null, outerPos)
      else {
        if (ev.tailstrict) {
          argVal1.force
          argVal2.force
          argVal3.force
        }
        evalRhs(Array(argVal1, argVal2, argVal3), ev, outerPos)
      }

    /** Specialize a call to this function in the optimizer. Must return either `null` to leave the
     * call-site as it is or a pair of a (possibly different) `Builtin` and the arguments to pass
     * to it (usually a subset of the supplied arguments).
     * @param args the positional arguments for this function call. Named arguments and defaults have
     *             already been resolved. */
    def specialize(args: Array[Expr], tailstrict: Boolean): (Builtin, Array[Expr]) = null

    /** Is this builtin safe to use in static evaluation */
    def staticSafe: Boolean = true
  }

  abstract class Builtin1(functionName: String, pn1: String, def1: Expr = null) extends Builtin(functionName: String, Array(pn1), if(def1 == null) null else Array(def1)) {
    final def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0).force, ev, pos)

    def evalRhs(arg1: Lazy, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val =
      if(namedNames == null && argVals.length == 1) evalRhs(argVals(0).force, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)
  }

  abstract class Builtin2(functionName: String, pn1: String, pn2: String, defs: Array[Expr] = null) extends Builtin(functionName: String, Array(pn1, pn2), defs) {
    final def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0).force, args(1).force, ev, pos)

    def evalRhs(arg1: Lazy, arg2: Lazy, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val =
      if(namedNames == null && argVals.length == 2)
        evalRhs(argVals(0).force, argVals(1).force, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)

    override def apply2(argVal1: Lazy, argVal2: Lazy, outerPos: Position)(implicit ev: EvalScope): Val =
      if(params.names.length == 2) evalRhs(argVal1.force, argVal2.force, ev, outerPos)
      else super.apply(Array(argVal1, argVal2), null, outerPos)
  }

  abstract class Builtin3(functionName: String, pn1: String, pn2: String, pn3: String, defs: Array[Expr] = null) extends Builtin(functionName: String, Array(pn1, pn2, pn3), defs) {
    final def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0).force, args(1).force, args(2).force, ev, pos)

    def evalRhs(arg1: Lazy, arg2: Lazy, arg3: Lazy, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val =
      if(namedNames == null && argVals.length == 3)
        evalRhs(argVals(0).force, argVals(1).force, argVals(2).force, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)
  }

  abstract class Builtin4(functionName: String, pn1: String, pn2: String, pn3: String, pn4: String, defs: Array[Expr] = null) extends Builtin(functionName: String, Array(pn1, pn2, pn3, pn4), defs) {
    final def evalRhs(args: Array[? <: Lazy], ev: EvalScope, pos: Position): Val =
      evalRhs(args(0).force, args(1).force, args(2).force, args(3).force, ev, pos)

    def evalRhs(arg1: Lazy, arg2: Lazy, arg3: Lazy, arg4: Lazy, ev: EvalScope, pos: Position): Val

    override def apply(argVals: Array[? <: Lazy], namedNames: Array[String], outerPos: Position)(implicit ev: EvalScope): Val =
      if(namedNames == null && argVals.length == 4)
        evalRhs(argVals(0).force, argVals(1).force, argVals(2).force, argVals(3).force, ev, outerPos)
      else super.apply(argVals, namedNames, outerPos)
  }
}

/**
  * [[EvalScope]] models the per-evaluator context that is propagated
  * throughout the Jsonnet evaluation.
  */
abstract class EvalScope extends EvalErrorScope with Ordering[Val] {
  def tailstrict: Boolean

  def visitExpr(expr: Expr)
               (implicit scope: ValScope): Val

  def materialize(v: Val): ujson.Value

  def equal(x: Val, y: Val): Boolean

  def compare(x: Val, y: Val): Int

  val emptyMaterializeFileScope = new FileScope(wd / "(materialize)")
  val emptyMaterializeFileScopePos = new Position(emptyMaterializeFileScope, -1)

  def settings: Settings
  def warn(e: Error): Unit
}
