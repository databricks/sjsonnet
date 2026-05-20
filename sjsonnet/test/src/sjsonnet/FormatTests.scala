package sjsonnet

import sjsonnet.Expr.Member.Visibility
import utest._

object FormatTests extends TestSuite {
  private val pos = new Position(null, 0)

  private implicit val scope: EvalScope = new EvalScope {
    def extVars: String => Option[Expr] = _ => None
    def importer: CachedImporter = new CachedImporter(Importer.empty)
    def wd: Path = DummyPath()
    def visitExpr(expr: Expr)(implicit scope: ValScope): Val =
      throw new UnsupportedOperationException("not used")
    def materialize(v: Val): ujson.Value =
      throw new UnsupportedOperationException("not used")
    def equal(x: Val, y: Val): Boolean = x == y
    def compare(x: Val, y: Val): Int = 0
    def settings: Settings = Settings.default
    def debugStats: DebugStats = null
    def trace(msg: String): Unit = ()
    def warn(e: Error): Unit = ()
  }

  def tests: Tests = Tests {
    test("simple named format preserves ascii-safe result for numeric values") {
      val fmt = new Format.PartialApplyFmt("hello %(x)s")
      val obj = Val.Obj.mk(
        pos,
        "x" -> new Val.Obj.ConstMember(add2 = false, Visibility.Normal, Val.Num(pos, 3))
      )
      val result = fmt.evalRhs(obj, scope, pos).asInstanceOf[Val.Str]
      result.str ==> "hello 3"
      result._asciiSafe ==> true
    }

    test("simple named format does not mark unsafe string values ascii-safe") {
      val fmt = new Format.PartialApplyFmt("hello %(x)s")
      val obj = Val.Obj.mk(
        pos,
        "x" -> new Val.Obj.ConstMember(add2 = false, Visibility.Normal, Val.Str(pos, "\""))
      )
      val result = fmt.evalRhs(obj, scope, pos).asInstanceOf[Val.Str]
      result.str ==> "hello \""
      result._asciiSafe ==> false
    }

    test("simple named format does not mark unsafe static literals ascii-safe") {
      val fmt = new Format.PartialApplyFmt("hello \"%(x)s")
      val obj = Val.Obj.mk(
        pos,
        "x" -> new Val.Obj.ConstMember(add2 = false, Visibility.Normal, Val.Num(pos, 3))
      )
      val result = fmt.evalRhs(obj, scope, pos).asInstanceOf[Val.Str]
      result.str ==> "hello \"3"
      result._asciiSafe ==> false
    }

    test("simple named format combines ascii-safety across multiple keys") {
      val safe = Val.Str.asciiSafe(pos, "safe")
      val unsafe = Val.Str(pos, "\\")
      val fmt = new Format.PartialApplyFmt("%(safe)s %(unsafe)s %(safe)s")
      val obj = Val.Obj.mk(
        pos,
        "safe" -> new Val.Obj.ConstMember(add2 = false, Visibility.Normal, safe),
        "unsafe" -> new Val.Obj.ConstMember(add2 = false, Visibility.Normal, unsafe)
      )
      val result = fmt.evalRhs(obj, scope, pos).asInstanceOf[Val.Str]
      result.str ==> "safe \\ safe"
      result._asciiSafe ==> false
    }
  }
}
