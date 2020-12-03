package sjsonnet

import java.net.URL

import com.fasterxml.jackson.databind.JsonNode
import org.openapi4j.core.model.v3.OAI3Context
import org.openapi4j.core.util.TreeUtil
import org.openapi4j.schema.validator.{ValidationContext, ValidationData}
import sjsonnet.Cli.Config

import scala.util.control.NonFatal
import org.openapi4j.schema.validator.v3.SchemaValidator

import scala.collection.mutable

class Validator private(schemas: Map[String, JsonNode]) {
  private val validators = mutable.Map.empty[String, SchemaValidator]

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
            Right(new SchemaValidator(new ValidationContext(ctx), tpref, n))
          }
        }
      } yield {
        validators.put(tpref, v)
        v
      }
  }

  def validate(json: ujson.Value, tpref: String): Either[String, ujson.Value] = {
    //println("--- validating ujson: "+json.toString())
    val jn = ujson.transform(json, new JacksonVisitor())
    validate(jn, tpref).map(_ => json)
  }

  def validate(json: JsonNode, tpref: String): Either[String, JsonNode] = {
    for {
      validator <- getValidator(tpref)
      _ <- {
        val vd = new ValidationData[Unit]()
        //println("--- validating: "+json.toString())
        validator.validate(json, vd)
        //println("--- results: "+vd.results())
        if(vd.isValid) Right(())
        else Left("Schema validation failed: "+vd.results())
      }
    } yield json
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
