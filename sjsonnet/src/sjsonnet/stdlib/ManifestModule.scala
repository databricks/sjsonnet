package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import java.io.StringWriter
import scala.collection.mutable

class ManifestModule extends AbstractFunctionModule {
  def name = "manifest"

  private val dummyPos: Position = new Position(null, 0)

  private object ManifestJson extends Val.Builtin1("manifestJson", "v") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Materializer.apply0(v.force, MaterializeJsonRenderer())(ev).toString)
  }

  private object ManifestJsonMinified extends Val.Builtin1("manifestJsonMinified", "value") {
    def evalRhs(v: Lazy, ev: EvalScope, pos: Position): Val =
      Val.Str(
        pos,
        Materializer
          .apply0(
            v.force,
            MaterializeJsonRenderer(indent = -1, newline = "", keyValueSeparator = ":")
          )(ev)
          .toString
      )
  }

  private object ManifestJsonEx
      extends Val.Builtin4(
        "manifestJsonEx",
        "value",
        "indent",
        "newline",
        "key_val_sep",
        Array(null, null, Val.Str(dummyPos, "\n"), Val.Str(dummyPos, ": "))
      ) {
    def evalRhs(
        v: Lazy,
        i: Lazy,
        newline: Lazy,
        keyValSep: Lazy,
        ev: EvalScope,
        pos: Position): Val =
      Val.Str(
        pos,
        Materializer
          .apply0(
            v.force,
            MaterializeJsonRenderer(
              indent = i.force.asString.length,
              newline = newline.force.asString,
              keyValueSeparator = keyValSep.force.asString
            )
          )(ev)
          .toString
      )
  }

  private object ParseJson extends Val.Builtin1("parseJson", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val = {
      try {
        ujson.StringParser.transform(str.force.asString, new ValVisitor(pos))
      } catch {
        case e: ujson.ParseException =>
          throw Error.fail("Invalid JSON: " + e.getMessage, pos)(ev)
      }
    }
  }

  private object ParseYaml extends Val.Builtin1("parseYaml", "str") {
    def evalRhs(str: Lazy, ev: EvalScope, pos: Position): Val =
      ujson.transform(Platform.yamlToJson(str.force.asString), new ValVisitor(pos))
  }

  private object ManifestTomlEx extends Val.Builtin2("manifestTomlEx", "value", "indent") {
    private def isTableArray(v: Val) = v.force match {
      case s: Val.Arr => s.length > 0 && s.asLazyArray.forall(_.isInstanceOf[Val.Obj])
      case _          => false
    }

    private def isSection(v: Val) = v.force.isInstanceOf[Val.Obj] || isTableArray(v.force)

    private def renderTableInternal(
        out: StringWriter,
        v: Val.Obj,
        cumulatedIndent: String,
        indent: String,
        path: Seq[String],
        indexedPath: Seq[String])(implicit ev: EvalScope): StringWriter = {
      val (sections, nonSections) =
        v.visibleKeyNames.partition(k => isSection(v.value(k, v.pos)(ev)))
      for (k <- nonSections.sorted(Util.CodepointStringOrdering)) {
        out.write(cumulatedIndent)
        out.write(TomlRenderer.escapeKey(k))
        out.write(" = ")
        Materializer.apply0(v.value(k, v.pos)(ev), new TomlRenderer(out, cumulatedIndent, indent))(
          ev
        )
      }
      out.write('\n')

      for (k <- sections.sorted(Util.CodepointStringOrdering)) {
        val v0 = v.value(k, v.pos, v)(ev)
        if (isTableArray(v0)) {
          for (i <- 0 until v0.asArr.length) {
            out.write(cumulatedIndent)
            renderTableArrayHeader(out, path :+ k)
            out.write('\n')
            renderTableInternal(
              out,
              v0.asArr.force(i).asObj,
              cumulatedIndent + indent,
              indent,
              path :+ k,
              indexedPath ++ Seq(k, i.toString)
            )
          }
        } else {
          out.write(cumulatedIndent)
          renderTableHeader(out, path :+ k)
          out.write('\n')
          renderTableInternal(
            out,
            v0.asObj,
            cumulatedIndent + indent,
            indent,
            path :+ k,
            indexedPath :+ k
          )
        }
      }
      out
    }

    private def renderTableHeader(out: StringWriter, path: Seq[String]) = {
      out.write('[')
      out.write(path.map(TomlRenderer.escapeKey).mkString("."))
      out.write(']')
      out
    }

    private def renderTableArrayHeader(out: StringWriter, path: Seq[String]) = {
      out.write('[')
      renderTableHeader(out, path)
      out.write(']')
      out
    }

    def evalRhs(v: Lazy, indent: Lazy, ev: EvalScope, pos: Position): Val = {
      val out = new StringWriter
      renderTableInternal(
        out,
        v.force.asObj,
        "",
        indent.force.asString,
        Seq.empty[String],
        Seq.empty[String]
      )(ev)
      Val.Str(pos, out.toString.strip)
    }
  }

  private object Lines extends Val.Builtin1("lines", "arr") {
    def evalRhs(v1: Lazy, ev: EvalScope, pos: Position): Val = {
      v1.force.asArr.foreach {
        case _: Val.Str | _: Val.Null => // donothing
        case x                        => Error.fail("Cannot call .lines on " + x.force.prettyName)
      }
      Val.Str(
        pos,
        Materializer
          .apply(v1.force)(ev)
          .asInstanceOf[ujson.Arr]
          .value
          .filter(_ != ujson.Null)
          .map {
            case ujson.Str(s) => s + "\n"
            case _            => ??? /* we ensure it's all strings above */
          }
          .mkString
      )
    }
  }

  private object DeepJoin extends Val.Builtin1("deepJoin", "arr") {
    def evalRhs(value: Lazy, ev: EvalScope, pos: Position): Val = {
      val out = new StringWriter()
      val q = new java.util.ArrayDeque[Lazy]()
      q.add(value)
      while (!q.isEmpty) {
        q.removeFirst().force match {
          case v: Val.Arr => v.asLazyArray.reverseIterator.foreach(q.push)
          case s: Val.Str => out.write(s.value)
          case s          => Error.fail("Cannot call deepJoin on " + s.prettyName)
        }
      }
      Val.Str(pos, out.toString)
    }
  }

  val functions: Seq[(String, Val.Func)] = Seq(
    builtin(ManifestJson),
    builtin(ManifestJsonMinified),
    builtin(ManifestJsonEx),
    builtin(ParseJson),
    builtin(ParseYaml),
    builtin(ManifestTomlEx),
    builtin(Lines),
    builtin(DeepJoin),
    builtin("manifestToml", "value") { (pos, ev, value: Val) =>
      ManifestTomlEx.evalRhs(value, Val.Str(pos, ""), ev, pos)
    },
    builtinWithDefaults(
      "manifestYamlDoc",
      "value" -> null,
      "indent_array_in_object" -> Val.False(dummyPos),
      "quote_keys" -> Val.True(dummyPos)
    ) { (args, pos, ev) =>
      val v = args(0)
      val indentArrayInObject = args(1) match {
        case Val.False(_) => false
        case Val.True(_)  => true
        case _ => Error.fail("indent_array_in_object has to be a boolean, got" + v.getClass)
      }
      val quoteKeys = args(2) match {
        case Val.False(_) => false
        case Val.True(_)  => true
        case _            => Error.fail("quote_keys has to be a boolean, got " + v.getClass)
      }
      Materializer
        .apply0(
          v,
          new YamlRenderer(indentArrayInObject = indentArrayInObject, quoteKeys = quoteKeys)
        )(ev)
        .toString
    },
    builtinWithDefaults(
      "manifestYamlStream",
      "value" -> null,
      "indent_array_in_object" -> Val.False(dummyPos),
      "c_document_end" -> Val.True(dummyPos),
      "quote_keys" -> Val.True(dummyPos)
    ) { (args, _, ev) =>
      val v = args(0)
      val indentArrayInObject = args(1) match {
        case Val.False(_) => false
        case Val.True(_)  => true
        case _ => Error.fail("indent_array_in_object has to be a boolean, got" + v.getClass)
      }
      val cDocumentEnd = args(2) match {
        case Val.False(_) => false
        case Val.True(_)  => true
        case _            => Error.fail("c_document_end has to be a boolean, got " + v.getClass)
      }
      val quoteKeys = args(3) match {
        case Val.False(_) => false
        case Val.True(_)  => true
        case _            => Error.fail("quote_keys has to be a boolean, got " + v.getClass)
      }
      v match {
        case arr: Val.Arr =>
          arr.asLazyArray
            .map { item =>
              Materializer
                .apply0(
                  item.force,
                  new YamlRenderer(indentArrayInObject = indentArrayInObject, quoteKeys = quoteKeys)
                )(ev)
                .toString
            }
            .mkString("---\n", "\n---\n", if (cDocumentEnd) "\n...\n" else "\n")
        case _ => Error.fail("manifestYamlStream only takes arrays, got " + v.getClass)
      }
    },
    builtin("manifestIni", "ini") { (pos, ev, v: Val) =>
      val materialized = Materializer(v)(ev)
      def render(x: ujson.Value) = x match {
        case ujson.Str(vv)  => vv
        case ujson.Num(vv)  => RenderUtils.renderDouble(vv)
        case ujson.Bool(vv) => vv.toString
        case ujson.Null     => "null"
        case _              => x.transform(new sjsonnet.Renderer())
      }
      def sect(x: ujson.Obj) = {
        // TODO remove the `toSeq` once this is fixed in scala3
        x.value.toSeq.flatMap {
          case (k, ujson.Arr(vs)) => vs.map(x => k + " = " + render(x))
          case (k, v)             => Seq(k + " = " + render(v))
        }
      }
      val lines = materialized.obj
        .get("main")
        .fold(Iterable[String]())(x => sect(x.asInstanceOf[ujson.Obj])) ++
        materialized.obj
          .get("sections")
          .fold(Iterable[String]())(x =>
            // TODO remove the `toSeq` once this is fixed in scala3
            x.obj.toSeq.flatMap { case (k, v) =>
              Seq("[" + k + "]") ++ sect(v.asInstanceOf[ujson.Obj])
            }
          )
      lines.flatMap(Seq(_, "\n")).mkString
    },
    builtin("manifestPython", "v") { (pos, ev, v: Val) =>
      Materializer.apply0(v, new PythonRenderer())(ev).toString
    },
    builtin("manifestPythonVars", "conf") { (pos, ev, v: Val.Obj) =>
      // TODO remove the `toSeq` once this is fixed in scala3
      Materializer(v)(ev).obj.toSeq.map { case (k, v) =>
        k + " = " + v.transform(new PythonRenderer()).toString + "\n"
      }.mkString
    },
    builtin("manifestXmlJsonml", "value") { (pos, ev, value: Val) =>
      import scalatags.Text.all.{value => _, _}
      def rec(v: ujson.Value): Frag = {
        v match {
          case ujson.Str(ss)                                                         => ss
          case ujson.Arr(mutable.Seq(ujson.Str(t), attrs: ujson.Obj, children @ _*)) =>
            tag(t)(
              // TODO remove the `toSeq` once this is fixed in scala3
              attrs.value.toSeq.map {
                case (k, ujson.Str(v)) => attr(k) := v

                // use ujson.write to make sure output number format is same as
                // google/jsonnet, e.g. whole numbers are printed without the
                // decimal point and trailing zero
                case (k, ujson.Num(v)) => attr(k) := ujson.write(v)

                case (k, v) => Error.fail("Cannot call manifestXmlJsonml on " + v.getClass)
              }.toSeq,
              children.map(rec)
            )
          case ujson.Arr(mutable.Seq(ujson.Str(t), children @ _*)) =>
            tag(t)(children.map(rec).toSeq)
          case x =>
            Error.fail("Cannot call manifestXmlJsonml on " + x.getClass)
        }
      }
      rec(Materializer(value)(ev)).render
    }
  )
}
