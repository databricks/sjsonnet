package sjsonnet

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node._
import upickle.core.{ArrVisitor, ObjVisitor, StringVisitor, Util, Visitor}

class JacksonVisitor(nf: JsonNodeFactory = JsonNodeFactory.instance) extends ujson.JsVisitor[JsonNode, JsonNode] { self =>

  def visitArray(length: Int, index: Int): ArrVisitor[JsonNode, JsonNode] = new ArrVisitor[JsonNode, JsonNode] {
    val n = new ArrayNode(nf)
    def subVisitor: Visitor[_, _] = self
    def visitValue(v: JsonNode, index: Int): Unit = n.add(v)
    def visitEnd(index: Int): JsonNode = n
  }

  def visitObject(length: Int, index: Int): ObjVisitor[JsonNode, JsonNode] = new ObjVisitor[JsonNode, JsonNode] {
    val n = new ObjectNode(nf)
    var key: String = null
    def visitKey(index: Int): Visitor[_, _] = StringVisitor
    def visitKeyValue(v: Any): Unit = key = v.toString
    def subVisitor: Visitor[_, _] = self
    def visitValue(v: JsonNode, index: Int): Unit = n.set(key, v)
    def visitEnd(index: Int): JsonNode = n
  }

  def visitNull(index: Int): JsonNode = NullNode.instance
  def visitFalse(index: Int): JsonNode = BooleanNode.FALSE
  def visitTrue(index: Int): JsonNode = BooleanNode.TRUE
  def visitString(s: CharSequence, index: Int): JsonNode = TextNode.valueOf(s.toString)

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): JsonNode =
    if (decIndex != -1 || expIndex != -1) new DoubleNode(s.toString.toDouble)
    else {
      val l = Util.parseIntegralNum(s, decIndex, expIndex, index)
      if(l <= Int.MaxValue && l >= Int.MinValue) new IntNode(l.toInt)
      else new LongNode(l)
    }
}
