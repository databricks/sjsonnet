package sjsonnet

/**
  * Typeclasses for easy conversion between [[Val]]s and Scala data types
  */
sealed trait ReadWriter[T]{
  def apply(t: Val, ev: EvalScope, fs: FileScope): Either[String, T]
  def write(t: T): Val
}
object ReadWriter{
  implicit object StringRead extends ReadWriter[String]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case Val.Str(s) => Right(s)
      case _ => Left("String")
    }
    def write(t: String) = Val.Str(t)
  }
  implicit object BooleanRead extends ReadWriter[Boolean]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case Val.True => Right(true)
      case Val.False => Right(false)
      case _ => Left("Boolean")
    }
    def write(t: Boolean) = Val.bool(t)
  }
  implicit object IntRead extends ReadWriter[Int]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case Val.Num(s) => Right(s.toInt)
      case _ => Left("Int")
    }
    def write(t: Int) = Val.Num(t)
  }
  implicit object DoubleRead extends ReadWriter[Double]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case Val.Num(s) => Right(s)
      case _ => Left("Number")
    }
    def write(t: Double) = Val.Num(t)
  }
  implicit object ValRead extends ReadWriter[Val]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = Right(t)
    def write(t: Val) = t
  }
  implicit object ObjRead extends ReadWriter[Val.Obj]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case v: Val.Obj => Right(v)
      case _ => Left("Object")
    }
    def write(t: Val.Obj) = t
  }
  implicit object ArrRead extends ReadWriter[Val.Arr]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case v: Val.Arr => Right(v)
      case _ => Left("Array")
    }
    def write(t: Val.Arr) = t
  }
  implicit object FuncRead extends ReadWriter[Val.Func]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case v: Val.Func => Right(v)
      case _ => Left("Function")
    }
    def write(t: Val.Func) = t
  }

  implicit object ApplyerRead extends ReadWriter[Applyer]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case v: Val.Func => Right(Applyer(v, ev, fs))
      case _ => Left("Function")
    }
    def write(t: Applyer) = t.f
  }
}
case class Applyer(f: Val.Func, ev: EvalScope, fs: FileScope){
  def apply(args: Val.Lazy*) = {
    f.apply(args.map((None, _)), "(memory)", -1)(fs, ev)
  }
}