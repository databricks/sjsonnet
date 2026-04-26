package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

import java.io.StringWriter
import scala.collection.mutable

/**
 * Native implementations for Jsonnet standard-library entries in this module.
 *
 * Official Jsonnet stdlib documentation links for this module:
 *
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestJson std.manifestJson(value)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestJsonMinified std.manifestJsonMinified(value)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestJsonEx std.manifestJsonEx(value, indent, newline, key_val_sep)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-parseJson std.parseJson(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-parseYaml std.parseYaml(str)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestTomlEx std.manifestTomlEx(value, indent)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-lines std.lines(arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-deepJoin std.deepJoin(arr)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestYamlDoc std.manifestYamlDoc(value, indent_array_in_object=false, quote_keys=true)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestYamlStream std.manifestYamlStream(value, indent_array_in_object=false, c_document_end=false, quote_keys=true)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestIni std.manifestIni(ini)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestPython std.manifestPython(v)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestPythonVars std.manifestPythonVars(conf)]]
 *   - [[https://jsonnet.org/ref/stdlib.html#std-manifestXmlJsonml std.manifestXmlJsonml(value)]]
 */
object ManifestModule extends AbstractFunctionModule {
  def name = "manifest"

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-manifestJson std.manifestJson(value)]].
   *
   * Since: 0.10.0. Group: Manifestation.
   *
   * Convert the given object to a JSON form. Under the covers, it calls std.manifestJsonEx with a
   * 4-space indent.
   */
  private object ManifestJson extends Val.Builtin1("manifestJson", "v") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.Str(pos, Materializer.apply0(v.value, MaterializeJsonRenderer())(ev).toString)
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-manifestJsonMinified std.manifestJsonMinified(value)]].
   *
   * Since: 0.18.0. Group: Manifestation.
   *
   * Convert the given object to a minified JSON form. Under the covers, it calls
   * std.manifestJsonEx.
   */
  private object ManifestJsonMinified extends Val.Builtin1("manifestJsonMinified", "value") {
    def evalRhs(v: Eval, ev: EvalScope, pos: Position): Val =
      Val.Str(
        pos,
        Materializer
          .apply0(
            v.value,
            MaterializeJsonRenderer(indent = -1, newline = "", keyValueSeparator = ":")
          )(ev)
          .toString
      )
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-manifestJsonEx std.manifestJsonEx(value, indent, newline, key_val_sep)]].
   *
   * Since: 0.10.0. Group: Manifestation.
   *
   * Convert the given object to a JSON form. indent is a string containing one or more whitespaces
   * that are used for indentation. newline is by default \n and is inserted where a newline would
   * normally be used to break long lines. key_val_sep is used to separate the key and value of an
   * object field.
   */
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
        v: Eval,
        i: Eval,
        newline: Eval,
        keyValSep: Eval,
        ev: EvalScope,
        pos: Position): Val =
      Val.Str(
        pos,
        Materializer
          .apply0(
            v.value,
            MaterializeJsonRenderer(
              indent = i.value.asString.length,
              newline = newline.value.asString,
              keyValueSeparator = keyValSep.value.asString
            )
          )(ev)
          .toString
      )
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-parseJson std.parseJson(str)]].
   *
   * Since: 0.13.0. Group: Parsing.
   *
   * Parses a JSON string.
   */
  private object ParseJson extends Val.Builtin1("parseJson", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
      try {
        ujson.StringParser.transform(str.value.asString, new ValVisitor(pos))
      } catch {
        case e: ujson.ParseException =>
          throw Error.fail("Invalid JSON: " + e.getMessage, pos)(ev)
      }
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-parseYaml std.parseYaml(str)]].
   *
   * Since: 0.18.0. Group: Parsing.
   *
   * Parses a YAML string. This is provided as a "best-effort" mechanism and should not be relied on
   * to provide a fully standards compliant YAML parser. YAML is a superset of JSON, consequently
   * "downcasting" or manifestation of YAML into JSON or Jsonnet values will only succeed when using
   * the subset of YAML that is compatible with JSON. The parser does not support YAML documents
   * with scalar values at the root. The root node of a YAML document must start with either a YAML
   * sequence or map to be successfully parsed.
   */
  private object ParseYaml extends Val.Builtin1("parseYaml", "str") {
    def evalRhs(str: Eval, ev: EvalScope, pos: Position): Val = {
      val input = str.value.asString
      if (input.isBlank) {
        return Val.Null(pos)
      }
      ujson.transform(Platform.yamlToJson(input), new ValVisitor(pos))
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-manifestTomlEx std.manifestTomlEx(value, indent)]].
   *
   * Since: 0.18.0. Group: Manifestation.
   *
   * Convert the given object to a TOML form. indent is a string containing one or more whitespaces
   * that are used for indentation.
   */
  private object ManifestTomlEx extends Val.Builtin2("manifestTomlEx", "value", "indent") {
    private def isTableArray(v: Val) = v.value match {
      case s: Val.Arr => s.length > 0 && s.asLazyArray.forall(_.isInstanceOf[Val.Obj])
      case _          => false
    }

    private def isSection(v: Val) = v.value.isInstanceOf[Val.Obj] || isTableArray(v.value)

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
              v0.asArr.value(i).asObj,
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

    def evalRhs(v: Eval, indent: Eval, ev: EvalScope, pos: Position): Val = {
      val out = new StringWriter
      renderTableInternal(
        out,
        v.value.asObj,
        "",
        indent.value.asString,
        Seq.empty[String],
        Seq.empty[String]
      )(ev)
      Val.Str(pos, out.toString.strip)
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-lines std.lines(arr)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Concatenate an array of strings into a text file with newline characters after each string.
   * This is suitable for constructing bash scripts and the like.
   */
  private object Lines extends Val.Builtin1("lines", "arr") {
    def evalRhs(v1: Eval, ev: EvalScope, pos: Position): Val = {
      v1.value.asArr.foreach {
        case _: Val.Str | _: Val.Null => // donothing
        case x                        => Error.fail("Cannot call .lines on " + x.value.prettyName)
      }
      Val.Str(
        pos,
        Materializer
          .apply(v1.value)(ev)
          .asInstanceOf[ujson.Arr]
          .value
          .filter(_ != ujson.Null)
          .map {
            case ujson.Str(s) => s + "\n"
            case _            =>
              throw new RuntimeException("Unexpected") /* we ensure it's all strings above */
          }
          .mkString
      )
    }
  }

  /**
   * [[https://jsonnet.org/ref/stdlib.html#std-deepJoin std.deepJoin(arr)]].
   *
   * Since: 0.10.0. Group: Arrays.
   *
   * Concatenate an array containing strings and arrays to form a single string. If arr is a string,
   * it is returned unchanged. If it is an array, it is flattened and the string elements are
   * concatenated together with no separator.
   */
  private object DeepJoin extends Val.Builtin1("deepJoin", "arr") {
    def evalRhs(value: Eval, ev: EvalScope, pos: Position): Val = {
      val out = new StringWriter()
      val q = new java.util.ArrayDeque[Eval]()
      q.add(value)
      while (!q.isEmpty) {
        q.removeFirst().value match {
          case v: Val.Arr =>
            val inner = v.asLazyArray
            var i = inner.length - 1
            while (i >= 0) { q.push(inner(i)); i -= 1 }
          case s: Val.Str => out.write(s.str)
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
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-manifestYamlDoc std.manifestYamlDoc(value, indent_array_in_object=false, quote_keys=true)]].
     *
     * Since: 0.10.0. Group: Manifestation.
     *
     * Convert the given value to a YAML form. Note that std.manifestJson could also be used for
     * this purpose, because any JSON is also valid YAML. But this function will produce more
     * canonical-looking YAML.
     *
     * The indent_array_in_object param adds additional indentation which some people may find
     * easier to read.
     *
     * The quote_keys parameter controls whether YAML identifiers are always quoted or only when
     * necessary.
     */
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
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-manifestYamlStream std.manifestYamlStream(value, indent_array_in_object=false, c_document_end=false, quote_keys=true)]].
     *
     * Since: 0.10.0. Group: Manifestation.
     *
     * Given an array of values, emit a YAML "stream", which is a sequence of documents separated by
     * --- and ending with ....
     *
     * The indent_array_in_object and quote_keys params are the same as in manifestYamlDoc.
     *
     * The c_document_end param adds the optional terminating ....
     */
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
          val items = arr.asLazyArray
          val sb = new java.lang.StringBuilder()
          var i = 0
          while (i < items.length) {
            if (i > 0) sb.append("\n---\n")
            else sb.append("---\n")
            sb.append(
              Materializer
                .apply0(
                  items(i).value,
                  new YamlRenderer(indentArrayInObject = indentArrayInObject, quoteKeys = quoteKeys)
                )(ev)
                .toString
            )
            i += 1
          }
          if (cDocumentEnd) sb.append("\n...\n") else sb.append('\n')
          sb.toString
        case _ => Error.fail("manifestYamlStream only takes arrays, got " + v.getClass)
      }
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-manifestIni std.manifestIni(ini)]].
     *
     * Since: 0.10.0. Group: Manifestation.
     *
     * Convert the given structure to a string in INI format. This allows using Jsonnet's object
     * model to build a configuration to be consumed by an application expecting an INI file. The
     * data is in the form of a set of sections, each containing a key/value mapping.
     */
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
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-manifestPython std.manifestPython(v)]].
     *
     * Since: 0.10.0. Group: Manifestation.
     *
     * Convert the given value to a JSON-like form that is compatible with Python. The chief
     * differences are True / False / None instead of true / false / null.
     */
    builtin("manifestPython", "v") { (pos, ev, v: Val) =>
      Materializer.apply0(v, new PythonRenderer())(ev).toString
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-manifestPythonVars std.manifestPythonVars(conf)]].
     *
     * Since: 0.10.0. Group: Manifestation.
     *
     * Convert the given object to a JSON-like form that is compatible with Python. The key
     * difference to std.manifestPython is that the top level is represented as a list of Python
     * global variables.
     */
    builtin("manifestPythonVars", "conf") { (pos, ev, v: Val.Obj) =>
      // TODO remove the `toSeq` once this is fixed in scala3
      Materializer(v)(ev).obj.toSeq.map { case (k, v) =>
        k + " = " + v.transform(new PythonRenderer()).toString + "\n"
      }.mkString
    },
    /**
     * [[https://jsonnet.org/ref/stdlib.html#std-manifestXmlJsonml std.manifestXmlJsonml(value)]].
     *
     * Since: 0.10.0. Group: Manifestation.
     *
     * Convert the given JsonML-encoded value to a string containing the XML.
     *
     * JsonML is designed to preserve "mixed-mode content" (i.e., textual data outside of or next to
     * elements). This includes the whitespace needed to avoid having all the XML on one line, which
     * is meaningful in XML. In order to have whitespace in the XML output, it must be present in
     * the JsonML input.
     */
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
