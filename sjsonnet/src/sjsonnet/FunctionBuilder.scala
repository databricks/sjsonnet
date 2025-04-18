package sjsonnet

/**
 * Function building helpers for building functions of the Jsonnet language.
 * */
trait FunctionBuilder {
  def builtin(obj: Val.Builtin): (String, Val.Builtin) = (obj.functionName, obj)

  def builtin[R: ReadWriter, T1: ReadWriter](name: String, p1: String)
                                            (eval: (Position, EvalScope, T1) => R): (String, Val.Func) = {
    (name, new Val.Builtin1(name, p1) {
      def evalRhs(arg1: Lazy, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.force)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter](name: String, p1: String, p2: String)
                                                            (eval: (Position, EvalScope, T1, T2) => R): (String, Val.Func) = {
    (name, new Val.Builtin2(name, p1, p2) {
      def evalRhs(arg1: Lazy, arg2: Lazy, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.force)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.force)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter](name: String, p1: String, p2: String, p3: String)
                                                                            (eval: (Position, EvalScope, T1, T2, T3) => R): (String, Val.Func) = {
    (name, new Val.Builtin3(name, p1, p2, p3) {
      def evalRhs(arg1: Lazy, arg2: Lazy, arg3: Lazy, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.force)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.force)
        val v3: T3 = implicitly[ReadWriter[T3]].apply(arg3.force)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2, v3))
      }
    })
  }

  def builtin[R: ReadWriter, T1: ReadWriter, T2: ReadWriter, T3: ReadWriter, T4: ReadWriter]
  (name: String, p1: String, p2: String, p3: String, p4: String)
  (eval: (Position, EvalScope, T1, T2, T3, T4) => R): (String, Val.Func) = {
    (name, new Val.Builtin4(name, p1, p2, p3, p4) {
      def evalRhs(arg1: Lazy, arg2: Lazy, arg3: Lazy, arg4: Lazy, ev: EvalScope, outerPos: Position): Val = {
        //println("--- calling builtin: "+name)
        val v1: T1 = implicitly[ReadWriter[T1]].apply(arg1.force)
        val v2: T2 = implicitly[ReadWriter[T2]].apply(arg2.force)
        val v3: T3 = implicitly[ReadWriter[T3]].apply(arg3.force)
        val v4: T4 = implicitly[ReadWriter[T4]].apply(arg4.force)
        implicitly[ReadWriter[R]].write(outerPos, eval(outerPos, ev, v1, v2, v3, v4))
      }
    })
  }

  /**
   * Helper function that can define a built-in function with default parameters
   *
   * Arguments of the eval function are (args, ev)
   */
  def builtinWithDefaults[R: ReadWriter](name: String, params: (String, Val.Literal)*)
                                        (eval: (Array[Val], Position, EvalScope) => R): (String, Val.Func) = {
    name -> new Val.Builtin(name, params.map(_._1).toArray, params.map(_._2).toArray) {
      def evalRhs(args: Array[_ <: Lazy], ev: EvalScope, pos: Position): Val =
        implicitly[ReadWriter[R]].write(pos, eval(args.map(_.force), pos, ev))
    }
  }

  def stringChars(pos: Position, str: String): Val.Arr = {
    val a = new Array[Lazy](str.length)
    var i = 0
    while (i < a.length) {
      a(i) = new Val.Str(pos, String.valueOf(str.charAt(i)))
      i += 1
    }
    new Val.Arr(pos, a)
  }

  def getVisibleKeys(ev: EvalScope, v1: Val.Obj): Array[String] =
    maybeSortKeys(ev, v1.visibleKeyNames)

  def getAllKeys(ev: EvalScope, v1: Val.Obj): Array[String] =
    maybeSortKeys(ev, v1.allKeyNames)

  @inline def maybeSortKeys(ev: EvalScope, keys: Array[String]): Array[String] =
    if (ev.settings.preserveOrder) keys else keys.sorted

  def getObjValuesFromKeys(pos: Position, ev: EvalScope, v1: Val.Obj, keys: Array[String]): Val.Arr =
    new Val.Arr(pos, keys.map { k =>
      (() => v1.value(k, pos.noOffset)(ev)): Lazy
    })
}
