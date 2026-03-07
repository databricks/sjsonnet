package sjsonnet

import scala.annotation.switch

final class AstVisitProfiler {
  val oldDispatchArmCounts = new Array[Long](AstVisitProfiler.DispatchArmNames.length)
  val normalTagCounts = new Array[Long](ExprTags.Error + 1)
  val invalidNodeCounts = new Array[Long](AstVisitProfiler.InvalidNodeNames.length)

  @inline final def countVisit(e: Expr): Unit = {
    val tag = e.tag
    if (e.isInvalidVisitTag) {
      oldDispatchArmCounts(AstVisitProfiler.DispatchArm.Invalid) += 1
      invalidNodeCounts(AstVisitProfiler.invalidNodeIndex(tag)) += 1
    } else {
      oldDispatchArmCounts(AstVisitProfiler.dispatchArmIndex(tag)) += 1
      normalTagCounts(tag) += 1
    }
  }

  def snapshot(): AstVisitProfileSnapshot =
    AstVisitProfileSnapshot(
      oldDispatchArmCounts.clone(),
      normalTagCounts.clone(),
      invalidNodeCounts.clone()
    )
}

final case class AstVisitProfileSnapshot(
    oldDispatchArmCounts: Array[Long],
    normalTagCounts: Array[Long],
    invalidNodeCounts: Array[Long]) {
  def totalVisits: Long = oldDispatchArmCounts.foldLeft(0L)(_ + _)
}

object AstVisitProfiler {
  object DispatchArm {
    final val ValidId = 0
    final val BinaryOp = 1
    final val Select = 2
    final val Val = 3
    final val ApplyBuiltin0 = 4
    final val ApplyBuiltin1 = 5
    final val ApplyBuiltin2 = 6
    final val ApplyBuiltin3 = 7
    final val ApplyBuiltin4 = 8
    final val And = 9
    final val Or = 10
    final val UnaryOp = 11
    final val Apply1 = 12
    final val Lookup = 13
    final val Function = 14
    final val LocalExpr = 15
    final val Apply = 16
    final val IfElse = 17
    final val Apply3 = 18
    final val MemberList = 19
    final val Apply2 = 20
    final val AssertExpr = 21
    final val ApplyBuiltin = 22
    final val Comp = 23
    final val Arr = 24
    final val SelectSuper = 25
    final val LookupSuper = 26
    final val InSuper = 27
    final val ObjExtend = 28
    final val ObjComp = 29
    final val Slice = 30
    final val Import = 31
    final val Apply0 = 32
    final val ImportStr = 33
    final val ImportBin = 34
    final val Error = 35
    final val Invalid = 36
  }

  val DispatchArmNames: Array[String] = Array(
    "ValidId",
    "BinaryOp",
    "Select",
    "Val",
    "ApplyBuiltin0",
    "ApplyBuiltin1",
    "ApplyBuiltin2",
    "ApplyBuiltin3",
    "ApplyBuiltin4",
    "And",
    "Or",
    "UnaryOp",
    "Apply1",
    "Lookup",
    "Function",
    "LocalExpr",
    "Apply",
    "IfElse",
    "Apply3",
    "ObjBody.MemberList",
    "Apply2",
    "AssertExpr",
    "ApplyBuiltin",
    "Comp",
    "Arr",
    "SelectSuper",
    "LookupSuper",
    "InSuper",
    "ObjExtend",
    "ObjBody.ObjComp",
    "Slice",
    "Import",
    "Apply0",
    "ImportStr",
    "ImportBin",
    "Error",
    "Invalid"
  )

  val NormalTagNames: Array[String] = Array(
    "",
    "ValidId",
    "BinaryOp",
    "Select",
    "Val.Literal",
    "Val.Func",
    "ApplyBuiltin0",
    "ApplyBuiltin1",
    "ApplyBuiltin2",
    "ApplyBuiltin3",
    "ApplyBuiltin4",
    "And",
    "Or",
    "UnaryOp",
    "Apply1",
    "Lookup",
    "Function",
    "LocalExpr",
    "Apply",
    "IfElse",
    "Apply3",
    "ObjBody.MemberList",
    "Apply2",
    "AssertExpr",
    "ApplyBuiltin",
    "Comp",
    "Arr",
    "SelectSuper",
    "LookupSuper",
    "InSuper",
    "ObjExtend",
    "ObjBody.ObjComp",
    "Slice",
    "Import",
    "Apply0",
    "ImportStr",
    "ImportBin",
    "Error"
  )

  @inline private[sjsonnet] final def dispatchArmIndex(tag: Int): Int = {
    if (tag <= ExprTags.Select) tag - 1
    else if (tag <= ExprTags.`Val.Func`) DispatchArm.Val
    else tag - 2
  }

  object InvalidNode {
    final val Id = 0
    final val Self = 1
    final val Dollar = 2
    final val Super = 3
    final val Other = 4
  }

  val InvalidNodeNames: Array[String] = Array("Id", "Self", "$", "Super", "Other")

  @inline private[sjsonnet] final def invalidNodeIndex(tag: Int): Int = (tag: @switch) match {
    case ExprTags.Id    => InvalidNode.Id
    case ExprTags.Self  => InvalidNode.Self
    case ExprTags.`$`   => InvalidNode.Dollar
    case ExprTags.Super => InvalidNode.Super
    case _              => InvalidNode.Other
  }
}
