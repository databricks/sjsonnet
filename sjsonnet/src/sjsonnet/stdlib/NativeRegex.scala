package sjsonnet.stdlib

import sjsonnet.Expr.Member.Visibility
import sjsonnet.Val.Obj
import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

object NativeRegex extends AbstractFunctionModule {
  def name = "regex"

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

  val functions: Seq[(String, Val.Builtin)] = Seq(
    "regexPartialMatch" -> new Val.Builtin2("regexPartialMatch", "pattern", "str") {
      override def evalRhs(pattern: Eval, str: Eval, ev: EvalScope, pos: Position): Val = {
        regexPartialMatch(pos, pattern.value.asString, str.value.asString)
      }
    },
    "regexFullMatch" -> new Val.Builtin2("regexFullMatch", "pattern", "str") {
      override def evalRhs(pattern: Eval, str: Eval, ev: EvalScope, pos: Position): Val = {
        regexPartialMatch(pos, s"^${pattern.value.asString}$$", str.value.asString)
      }
    },
    "regexGlobalReplace" -> new Val.Builtin3("regexGlobalReplace", "str", "pattern", "to") {
      override def evalRhs(
          str: Eval,
          pattern: Eval,
          to: Eval,
          ev: EvalScope,
          pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.value.asString)
        val matcher = compiledPattern.matcher(str.value.asString)
        Val.Str(pos.noOffset, matcher.replaceAll(to.value.asString))
      }
    },
    "regexReplace" -> new Val.Builtin3("regexReplace", "str", "pattern", "to") {
      override def evalRhs(
          str: Eval,
          pattern: Eval,
          to: Eval,
          ev: EvalScope,
          pos: Position): Val = {
        val compiledPattern = Platform.getPatternFromCache(pattern.value.asString)
        val matcher = compiledPattern.matcher(str.value.asString)
        Val.Str(pos.noOffset, matcher.replaceFirst(to.value.asString))
      }
    },
    "regexQuoteMeta" -> new Val.Builtin1("regexQuoteMeta", "str") {
      override def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
        Val.Str(pos.noOffset, Platform.regexQuote(str.value.asString))
      }
    }
  )
}
