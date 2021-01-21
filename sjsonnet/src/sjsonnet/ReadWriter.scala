package sjsonnet

/**
  * Typeclasses for easy conversion between [[Val]]s and Scala data types
  */
sealed trait ReadWriter[T]{
  def apply(t: Val, ev: EvalScope, fs: FileScope): Either[String, T]
  def write(pos: Position, t: T): Val
}
object ReadWriter{
  implicit object StringRead extends ReadWriter[String]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case Val.Str(_, s) => Right(s)
      case _ => Left("String")
    }
    def write(pos: Position, t: String) = Val.Str(pos, t)
  }
  implicit object BooleanRead extends ReadWriter[Boolean]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case Val.True(_) => Right(true)
      case Val.False(_) => Right(false)
      case _ => Left("Boolean")
    }
    def write(pos: Position, t: Boolean) = Val.bool(pos, t)
  }
  implicit object IntRead extends ReadWriter[Int]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case Val.Num(_, s) => Right(s.toInt)
      case _ => Left("Int")
    }
    def write(pos: Position, t: Int) = Val.Num(pos, t)
  }
  implicit object DoubleRead extends ReadWriter[Double]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case Val.Num(_, s) => Right(s)
      case _ => Left("Number")
    }
    def write(pos: Position, t: Double) = Val.Num(pos, t)
  }
  implicit object ValRead extends ReadWriter[Val]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = Right(t)
    def write(pos: Position, t: Val) = t
  }
  implicit object ObjRead extends ReadWriter[Val.Obj]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case v: Val.Obj => Right(v)
      case _ => Left("Object")
    }
    def write(pos: Position, t: Val.Obj) = t
  }
  implicit object ArrRead extends ReadWriter[Val.Arr]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case v: Val.Arr => Right(v)
      case _ => Left("Array")
    }
    def write(pos: Position, t: Val.Arr) = t
  }
  implicit object FuncRead extends ReadWriter[Val.Func]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case v: Val.Func => Right(v)
      case _ => Left("Function")
    }
    def write(pos: Position, t: Val.Func) = t
  }

  implicit object ApplyerRead extends ReadWriter[Applyer]{
    def apply(t: Val, ev: EvalScope, fs: FileScope) = t match{
      case v: Val.Func => Right(Applyer(v, ev, fs))
      case _ => Left("Function")
    }
    def write(pos: Position, t: Applyer) = t.f
  }
}
case class Applyer(f: Val.Func, ev: EvalScope, fs: FileScope){
  def apply(args: Val.Lazy*) = {
    f.apply(args.map((None, _)), "(memory)", -1)(fs, ev)
  }
}