package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Val.Obj

object StdRegex {
  def regexPartialMatch(pos: Position, pattern: String, str: String): Val = {
    val compiledPattern = Platform.getPatternFromCache(pattern)
    val matcher = compiledPattern.matcher(str)
    var returnStr: Val = null
    val groupCount = matcher.groupCount()
    val captures = Array.newBuilder[Val]
    captures.sizeHint(groupCount)

    while (matcher.find()) {
      if (returnStr == null) {
        val m = matcher.group(0)
        if (m != null) {
          returnStr = Val.Str(pos.noOffset, matcher.group(0))
        } else {
          returnStr = Val.Str(pos.noOffset, "")
        }
      }
      for (i <- 1 to groupCount) {
        val m = matcher.group(i)
        if (m == null) {
          captures += Val.Str(pos.noOffset, "")
        } else {
          captures += Val.Str(pos.noOffset, m)
        }
      }
    }
    if (returnStr == null) {
      return Val.Null(pos.noOffset)
    }

    val result = captures.result()
    val namedCaptures = Platform.getNamedGroupsMap(compiledPattern).map {
      case (k, v) =>
        k -> new Obj.ConstMember(true, Visibility.Normal, result(v - 1))
    }.toSeq

    Val.Obj.mk(pos.noOffset,
      "string" -> new Obj.ConstMember(true, Visibility.Normal, returnStr),
      "captures" -> new Obj.ConstMember(true, Visibility.Normal, new Val.Arr(pos.noOffset, result)),
      "namedCaptures" -> new Obj.ConstMember(true, Visibility.Normal, Val.Obj.mk(pos.noOffset, namedCaptures: _*))
    )
  }

  def functions: Map[String, Val.Builtin] = Map(
    "regexPartialMatch" -> new Val.Builtin2("regexPartialMatch", "pattern", "str") {
      override def evalRhs(pattern: Val, str: Val, ev: EvalScope, pos: Position): Val = {
        regexPartialMatch(pos, pattern.asString, str.asString)
      }
    },
    "regexFullMatch" -> new Val.Builtin2("regexFullMatch", "pattern", "str") {
      override def evalRhs(pattern: Val, str: Val, ev: EvalScope, pos: Position): Val = {
        regexPartialMatch(pos, s"^${pattern.asString}$$", str.asString)
      }
    },
    "regexGlobalReplace" -> new Val.Builtin3("regexGlobalReplace", "str", "pattern", "to") {
      override def evalRhs(str: Val, pattern: Val, to: Val, ev: EvalScope, pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.asString)
        val matcher = compiledPattern.matcher(str.asString)
        Val.Str(pos.noOffset, matcher.replaceAll(to.asString))
      }
    },
    "regexReplace" -> new Val.Builtin3("regexReplace", "str", "pattern", "to") {
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
