package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Val.Obj

object StdRegex {
  def functions: Map[String, Val.Builtin] = Map(
    "regexPartialMatch" -> new Val.Builtin2("regexPartialMatch", "pattern", "str") {
      override def evalRhs(pattern: Val, str: Val, ev: EvalScope, pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.asString)
        val matcher = compiledPattern.matcher(str.asString)
        var returnStr: Val = Val.Null(pos.noOffset)
        val captures = Array.newBuilder[Val.Str]
        val groupCount = matcher.groupCount()
        while (matcher.find()) {
          if (returnStr.isInstanceOf[Val.Null]) {
            returnStr = Val.Str(pos.noOffset, matcher.group(0))
          }
          for (i <- 1 to groupCount) {
            captures += Val.Str(pos.noOffset, matcher.group(i))
          }
        }
        val result = captures.result()
        Val.Obj.mk(pos.noOffset,
          "string" -> new Obj.ConstMember(true, Visibility.Normal, returnStr),
          "captures" -> new Obj.ConstMember(true, Visibility.Normal, new Val.Arr(pos.noOffset, result))
        )
      }
    },
    "regexFullMatch" -> new Val.Builtin2("regexFullMatch", "pattern", "str") {
      override def evalRhs(pattern: Val, str: Val, ev: EvalScope, pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.asString)
        val matcher = compiledPattern.matcher(str.asString)
        if (!matcher.matches()) {
          Val.Obj.mk(pos.noOffset,
            "string" -> new Obj.ConstMember(true, Visibility.Normal, Val.Null(pos.noOffset)),
            "captures" -> new Obj.ConstMember(true, Visibility.Normal, new Val.Arr(pos.noOffset, Array.empty[Lazy]))
          )
        } else {
          val captures = Array.newBuilder[Val.Str]
          val groupCount = matcher.groupCount()
          for (i <- 0 to groupCount) {
            captures += Val.Str(pos.noOffset, matcher.group(i))
          }
          val result = captures.result()
          Val.Obj.mk(pos.noOffset,
            "string" -> new Obj.ConstMember(true, Visibility.Normal, result.head),
            "captures" -> new Obj.ConstMember(true, Visibility.Normal, new Val.Arr(pos.noOffset, result.drop(1)))
          )
        }
      }
    },
    "regexGlobalReplace" -> new Val.Builtin3("regexGlobalReplace", "str", "pattern", "to") {
      override def evalRhs(str: Val, pattern: Val, to: Val, ev: EvalScope, pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.asString)
        val matcher = compiledPattern.matcher(str.asString)
        Val.Str(pos.noOffset, matcher.replaceAll(to.asString))
      }
    },
    "regexReplace" -> new Val.Builtin3("regexGlobalReplace", "str", "pattern", "to") {
      override def evalRhs(str: Val, pattern: Val, to: Val, ev: EvalScope, pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.asString)
        val matcher = compiledPattern.matcher(str.asString)
        Val.Str(pos.noOffset, matcher.replaceFirst(to.asString))
      }
    },
    "regexQuoteMeta" -> new Val.Builtin1("regexQuoteMeta", "str") {
      override def evalRhs(str: Val, ev: EvalScope, pos: Position): Val = {
        Val.Str(pos.noOffset, Platform.regexQuote(str.asString))
      }
    }
  )
}
