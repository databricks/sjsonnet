package sjsonnet

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Val.Obj

object StdRegex {
  private final def regexPartialMatch(pos: Position, pattern: String, str: String): Val = {
    val compiledPattern = Platform.getPatternFromCache(pattern)
    val matcher = compiledPattern.matcher(str)

    if (matcher.find()) {
      val captures = Range.Int
        .inclusive(1, matcher.groupCount(), 1)
        .map(i => Val.Str(pos.noOffset, Option(matcher.group(i)).getOrElse("")))
        .toArray
      val namedCaptures = Platform
        .getNamedGroupsMap(compiledPattern)
        .map { case (k, v) =>
          k -> new Obj.ConstMember(true, Visibility.Normal, captures(v - 1))
        }
        .toSeq

      Val.Obj.mk(
        pos.noOffset,
        "string" -> new Obj.ConstMember(true, Visibility.Normal, Val.Str(pos.noOffset, str)),
        "captures" -> new Obj.ConstMember(
          true,
          Visibility.Normal,
          Val.Arr(pos.noOffset, captures)
        ),
        "namedCaptures" -> new Obj.ConstMember(
          true,
          Visibility.Normal,
          Val.Obj.mk(pos.noOffset, namedCaptures: _*)
        )
      )
    } else {
      Val.Null(pos.noOffset)
    }
  }

  def functions: Map[String, Val.Builtin] = Map(
    "regexPartialMatch" -> new Val.Builtin2("regexPartialMatch", "pattern", "str") {
      override def evalRhs(pattern: Lazy, str: Lazy, ev: EvalScope, pos: Position): Val = {
        regexPartialMatch(pos, pattern.force.asString, str.force.asString)
      }
    },
    "regexFullMatch" -> new Val.Builtin2("regexFullMatch", "pattern", "str") {
      override def evalRhs(pattern: Lazy, str: Lazy, ev: EvalScope, pos: Position): Val = {
        regexPartialMatch(pos, s"^${pattern.force.asString}$$", str.force.asString)
      }
    },
    "regexGlobalReplace" -> new Val.Builtin3("regexGlobalReplace", "str", "pattern", "to") {
      override def evalRhs(
          str: Lazy,
          pattern: Lazy,
          to: Lazy,
          ev: EvalScope,
          pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.force.asString)
        val matcher = compiledPattern.matcher(str.force.asString)
        Val.Str(pos.noOffset, matcher.replaceAll(to.force.asString))
      }
    },
    "regexReplace" -> new Val.Builtin3("regexReplace", "str", "pattern", "to") {
      override def evalRhs(
          str: Lazy,
          pattern: Lazy,
          to: Lazy,
          ev: EvalScope,
          pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.force.asString)
        val matcher = compiledPattern.matcher(str.force.asString)
        Val.Str(pos.noOffset, matcher.replaceFirst(to.force.asString))
      }
    },
    "regexQuoteMeta" -> new Val.Builtin1("regexQuoteMeta", "str") {
      override def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val = {
        Val.Str(pos.noOffset, Platform.regexQuote(str.force.asString))
      }
    }
  )
}
