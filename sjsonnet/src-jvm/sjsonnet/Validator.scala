package sjsonnet

import java.net.URL

import com.fasterxml.jackson.databind.JsonNode
import org.openapi4j.core.model.v3.OAI3SchemaKeywords.{TYPE_ARRAY, TYPE_BOOLEAN, TYPE_INTEGER, TYPE_NUMBER, TYPE_OBJECT, TYPE_STRING}
import org.openapi4j.core.model.v3.{OAI3, OAI3Context}
import org.openapi4j.core.util.TreeUtil
import org.openapi4j.core.validation.{ValidationResult, ValidationResults, ValidationSeverity}
import org.openapi4j.schema.validator.{BaseJsonValidator, JsonValidator, ValidationContext, ValidationData}
import org.openapi4j.schema.validator.v3.{SchemaValidator, ValidatorInstance}
import sjsonnet.Cli.Config

import scala.util.control.NonFatal
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class Validator private(schemas: Map[String, JsonNode]) {
  private val validators = mutable.Map.empty[String, SchemaValidator]

  object intOrStringFormatVal extends ValidatorInstance {
    def apply(ctx: ValidationContext[OAI3], schemaNode: JsonNode, schemaParentNode: JsonNode, parentSchema: SchemaValidator): JsonValidator = {
      val isIntOrString = schemaNode.textValue() == "int-or-string"
      new BaseJsonValidator[OAI3](ctx, schemaNode, schemaParentNode, parentSchema) {
        override def validate(valueNode: JsonNode, validation: ValidationData[_]): Boolean = {
          if(isIntOrString) false // skip default validator (which doesn't know int-or-string)
          else true
        }
      }
    }
  }

  object intOrStringTypeVal extends ValidatorInstance {
    private val ERR = new ValidationResult(ValidationSeverity.ERROR, 1027, "Type expected 'int' or 'string', found '%s'.")
    private val CRUMB_INFO = new ValidationResults.CrumbInfo("x-int-or-string", true)

    def apply(ctx: ValidationContext[OAI3], schemaNode: JsonNode, schemaParentNode: JsonNode, parentSchema: SchemaValidator): JsonValidator = {
      val isIntOrString = Option(schemaParentNode.get("format")).map(_.textValue()).getOrElse(null) == "int-or-string"
      new BaseJsonValidator[OAI3](ctx, schemaNode, schemaParentNode, parentSchema) {
        override def validate(valueNode: JsonNode, validation: ValidationData[_]): Boolean = {
          if(isIntOrString) {
            if(!valueNode.isTextual && !valueNode.isIntegralNumber) {
              validation.add(CRUMB_INFO, ERR, getTypeFromValue(valueNode))
            }
            false
          } else true
        }
      }
    }

    private def getTypeFromValue(valueNode: JsonNode): String = {
      if (valueNode.isContainerNode) {
        if (valueNode.isObject) TYPE_OBJECT else TYPE_ARRAY
      }
      else if (valueNode.isTextual) TYPE_STRING
      else if (valueNode.isIntegralNumber) TYPE_INTEGER
      else if (valueNode.isNumber) TYPE_NUMBER
      else if (valueNode.isBoolean) TYPE_BOOLEAN
      else "null"
    }
  }

  def getValidator(tpref: String): Either[String, SchemaValidator] = validators.get(tpref) match {
    case Some(v) => Right(v)
    case _ =>
      for {
        parsed <- (tpref.split('#') match {
          case Array(doc, frag) => Right((doc, frag))
          case _ => Left(s"Type ref $tpref must be of the form document#fragment")
        }): Either[String, (String, String)]
        (doc, frag) = parsed
        root <- schemas.get(doc).toRight(s"OpenAPI schema $doc in type ref $tpref not defined")
        v <- {
          val n = root.at(frag)
          if(n.isMissingNode) Left(s"Type ref $tpref not found in schema $doc")
          else {
            val ctx = new OAI3Context(new URL("file:/"), root)
            val valctx = new ValidationContext[OAI3](ctx)
            valctx.addValidator("format", intOrStringFormatVal)
            valctx.addValidator("type", intOrStringTypeVal)
            Right(new SchemaValidator(valctx, tpref, n))
          }
        }
      } yield {
        validators.put(tpref, v)
        v
      }
  }

  def validate(json: ujson.Value, tpref: String): Either[String, (Boolean, Seq[ValidationError])] = {
    //println("--- validating ujson: "+json.toString())
    validate(ujson.transform(json, new JacksonVisitor()), tpref)
  }

  def validate(json: JsonNode, tpref: String): Either[String, (Boolean, Seq[ValidationError])] = {
    for {
      validator <- getValidator(tpref)
      res <- {
        val vd = new ValidationData[Unit]()
        //println("--- validating: "+json.toString())
        validator.validate(json, vd)
        //println("--- results: "+vd.results())
        val errors = vd.results().items().asScala.iterator.collect {
          case item if item.severity().ge(ValidationSeverity.WARNING) =>
            var path = item.dataCrumbs()
            if(path.startsWith(tpref)) path = path.substring(tpref.length)
            if(path.startsWith(".")) path = path.substring(1)
            var spath = item.schemaCrumbs()
            if(spath.startsWith(tpref)) spath = s"<$tpref>" + spath.substring(tpref.length)
            ValidationError(
              if(item.severity().ge(ValidationSeverity.ERROR)) ValidationError.Error else ValidationError.Warning,
              JsonPath(path.split('.')),
              item.message(),
              SchemaPath(spath)
            )
        }.toSeq
        Right((vd.isValid, errors))
      }
    } yield res
  }
}

object Validator {
  def apply(config: Config): Either[String, Validator] = {
    try {
      val schemas = config.openApiSchemas.iterator.map { case (k, v) =>
        (k, TreeUtil.load(v.toIO.toURI.toURL))
      }.toMap
      Right(new Validator(schemas))
    } catch { case NonFatal(ex) => Left(ex.getMessage) }
  }
}

case class ValidationError(sev: ValidationError.Severity, at: JsonPath, msg: String, schemaPath: SchemaPath)

object ValidationError {
  sealed trait Severity
  case object Error extends Severity
  case object Warning extends Severity
  case object Info extends Severity
}

case class JsonPath(segments: Seq[String]) {
  override def toString: String = segments.mkString(".")
  def parent: Option[JsonPath] =
    if(segments.isEmpty) None else Some(JsonPath(segments.init))
}

case class SchemaPath(raw: String) {
  override def toString: String = raw
  val segments: Seq[String] = {
    val b = new ArrayBuffer[String]()
    var i = 0
    var start = 0
    var nesting = 0
    while(i < raw.length) {
      raw.charAt(i) match {
        case '.' if nesting == 0 =>
          if(i - start > 0) b += raw.substring(start, i)
          start = i + 1
        case '<' => nesting += 1
        case '>' => nesting -= 1
        case _ =>
      }
      i += 1
    }
    if(i - start > 0) b += raw.substring(start, i)
    b.toSeq
  }
  val grouped: Seq[String] = {
    val b = new ArrayBuffer[String]()
    segments.foreach { s =>
      if(b.isEmpty || s.startsWith("<")) b += s
      else b.update(b.length-1, b.last + "." + s)
    }
    b.toSeq
  }
}
