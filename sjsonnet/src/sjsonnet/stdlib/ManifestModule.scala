package sjsonnet.stdlib

import sjsonnet._
import sjsonnet.functions.AbstractFunctionModule

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
      Val.Str(pos, Materializer.apply0(v.value, new FastMaterializeJsonRenderer())(ev).toString)
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
            new FastMaterializeJsonRenderer(indent = -1, newline = "", keyValueSeparator = ":")
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
            new FastMaterializeJsonRenderer(
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
    private def isTableArray(v: Val) = v match {
      case s: Val.Arr =>
        if (s.length == 0) false
        else {
          var i = 0
          var allObjects = true
          while (i < s.length && allObjects) {
            allObjects = s.value(i).isInstanceOf[Val.Obj]
            i += 1
          }
          allObjects
        }
      case _ => false
    }

    private def isSection(v: Val) = v.isInstanceOf[Val.Obj] || isTableArray(v)

    private def renderTableInternal(
        out: StringBuilderWriter,
        v: Val.Obj,
        cumulatedIndent: String,
        indent: String,
        path: mutable.ArrayBuffer[String])(implicit ev: EvalScope): Boolean = {
      val keys = v.sortedVisibleKeyNames
      if (keys.length == 0) {
        return false
      }

      // Resolve each field once and cache the result: the value is needed twice
      // (to classify scalars vs sections, then to render). Iterating `keys` directly
      // also skips the binary search that mapped `visibleKeyNames` back into `keys`.
      val resolved = new Array[Val](keys.length)
      val sectionFlags = new Array[Boolean](keys.length)
      var keyIdx = 0
      while (keyIdx < keys.length) {
        val r = v.value(keys(keyIdx), v.pos)(ev)
        resolved(keyIdx) = r
        sectionFlags(keyIdx) = isSection(r)
        keyIdx += 1
      }

      val renderer = new TomlRenderer(out, cumulatedIndent, indent)
      var hasSimpleKV = false
      keyIdx = 0
      while (keyIdx < keys.length) {
        if (!sectionFlags(keyIdx)) {
          hasSimpleKV = true
          out.write(cumulatedIndent)
          TomlRenderer.writeEscapedKey(out, keys(keyIdx))
          out.write(" = ")
          Materializer.apply0(resolved(keyIdx), renderer)(ev)
        }
        keyIdx += 1
      }
      // childIndent depends only on cumulatedIndent + indent, so compute it once
      // instead of per section iteration.
      val childIndent = cumulatedIndent + indent
      var lastEndedWithNewline = hasSimpleKV
      keyIdx = 0
      while (keyIdx < keys.length) {
        if (sectionFlags(keyIdx)) {
          val k = keys(keyIdx)
          val v0 = resolved(keyIdx)
          path += k
          v0 match {
            case arr: Val.Arr =>
              var i = 0
              while (i < arr.length) {
                if (i == 0) {
                  if (lastEndedWithNewline) out.write('\n')
                  else out.write("\n\n")
                } else {
                  out.write('\n')
                }
                out.write(cumulatedIndent)
                renderTableArrayHeader(out, path)
                out.write('\n')
                renderTableInternal(
                  out,
                  arr.value(i).asObj,
                  childIndent,
                  indent,
                  path
                )
                i += 1
              }
              lastEndedWithNewline = true
            case obj: Val.Obj =>
              if (lastEndedWithNewline) out.write('\n')
              else out.write("\n\n")
              out.write(cumulatedIndent)
              renderTableHeader(out, path)
              val childHasContent = obj.sortedVisibleKeyNames.nonEmpty
              if (childHasContent) out.write('\n')
              renderTableInternal(
                out,
                obj,
                childIndent,
                indent,
                path
              )
              lastEndedWithNewline = childHasContent
            case _ =>
              ()
          }
          path.remove(path.length - 1)
        }
        keyIdx += 1
      }
      keys.nonEmpty
    }

    private def renderTableHeader(out: StringBuilderWriter, path: mutable.ArrayBuffer[String]) = {
      out.write('[')
      var i = 0
      while (i < path.length) {
        if (i != 0) out.write('.')
        TomlRenderer.writeEscapedKey(out, path(i))
        i += 1
      }
      out.write(']')
      out
    }

    private def renderTableArrayHeader(
        out: StringBuilderWriter,
        path: mutable.ArrayBuffer[String]) = {
      out.write('[')
      renderTableHeader(out, path)
      out.write(']')
      out
    }

    def evalRhs(v: Eval, indent: Eval, ev: EvalScope, pos: Position): Val = {
      // Pre-size at 1 KiB to skip the first ~6 doublings (16→1024) for typical TOML
      // outputs without overcommitting memory on small ones.
      val out = new StringBuilderWriter(1024)
      renderTableInternal(
        out,
        v.value.asObj,
        "",
        indent.value.asString,
        new mutable.ArrayBuffer[String](8)
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
      // Use the unsynchronized StringBuilderWriter: each Val.Str chunk triggers
      // out.write(...) in a tight loop, and StringWriter's backing StringBuffer
      // pays a monitor enter/exit per call. Same swap pattern as TomlRenderer (#875).
      val out = new StringBuilderWriter()
      val q = new java.util.ArrayDeque[Eval]()
      q.add(value)
      while (!q.isEmpty) {
        q.removeFirst().value match {
          case v: Val.Arr =>
            var i = v.length - 1
            while (i >= 0) { q.push(v.eval(i)); i -= 1 }
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
          val sb = new java.lang.StringBuilder()
          var i = 0
          while (i < arr.length) {
            if (i > 0) sb.append("\n---\n")
            else sb.append("---\n")
            sb.append(
              Materializer
                .apply0(
                  arr.value(i),
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
      val mainLines = materialized.obj
        .get("main")
        .fold(Iterable[String]())(x => sect(x.asInstanceOf[ujson.Obj]))
      // Official std.jsonnet accesses `ini.sections` directly, so a missing `sections`
      // field is an error rather than being silently treated as empty (issue #799).
      // `main` stays optional, matching `std.objectHas(ini, 'main')` in the upstream impl.
      val sectionLines = materialized.obj.get("sections") match {
        case Some(x) =>
          // TODO remove the `toSeq` once this is fixed in scala3
          x.obj.toSeq.flatMap { case (k, v) =>
            Seq("[" + k + "]") ++ sect(v.asInstanceOf[ujson.Obj])
          }
        case None =>
          Error.fail("Field does not exist: sections", pos)(ev)
      }
      val lines = mainLines ++ sectionLines
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
