package sjsonnet

/**
  * Typeclasses for easy conversion between [[Val]]s and Scala data types
  */
sealed abstract class ReadWriter[T] {
  protected[this] def fail(err: String, v: Val): Nothing =
    throw new Error.Delegate("Wrong parameter type: expected " + err + ", got " + v.prettyName)
  def apply(t: Val): T
  def write(pos: Position, t: T): Val
}
object ReadWriter{
  implicit object StringRead extends ReadWriter[String]{
    def apply(t: Val) = t match{
      case Val.Str(_, s) => s
      case _ => fail("String", t)
    }
    def write(pos: Position, t: String) = Val.Str(pos, t)
  }
  implicit object BooleanRead extends ReadWriter[Boolean]{
    def apply(t: Val) = t match{
      case Val.True(_) => true
      case Val.False(_) => false
      case _ => fail("Boolean", t)
    }
    def write(pos: Position, t: Boolean) = Val.bool(pos, t)
  }
  implicit object IntRead extends ReadWriter[Int]{
    def apply(t: Val) = t match{
      case Val.Num(_, s) => s.toInt
      case _ => fail("Int", t)
    }
    def write(pos: Position, t: Int) = Val.Num(pos, t)
  }
  implicit object DoubleRead extends ReadWriter[Double]{
    def apply(t: Val) = t match{
      case Val.Num(_, s) => s
      case _ => fail("Number", t)
    }
    def write(pos: Position, t: Double) = Val.Num(pos, t)
  }
  implicit object ValRead extends ReadWriter[Val]{
    def apply(t: Val) = t
    def write(pos: Position, t: Val) = t
  }
  implicit object ObjRead extends ReadWriter[Val.Obj]{
    def apply(t: Val) = t match{
      case v: Val.Obj => v
      case _ => fail("Object", t)
    }
    def write(pos: Position, t: Val.Obj) = t
  }
  implicit object ArrRead extends ReadWriter[Val.Arr]{
    def apply(t: Val) = t match{
      case v: Val.Arr => v
      case _ => fail("Array", t)
    }
    def write(pos: Position, t: Val.Arr) = t
  }
  implicit object FuncRead extends ReadWriter[Val.Func]{
    def apply(t: Val) = t match{
      case v: Val.Func => v
      case _ => fail("Function", t)
    }
    def write(pos: Position, t: Val.Func) = t
  }
}
