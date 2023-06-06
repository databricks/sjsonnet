package sjsonnet

import java.util

import ujson.JsVisitor
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable

/** Parse JSON directly into a literal `Val` */
class ValVisitor(pos: Position) extends JsVisitor[Val, Val] { self =>
  def visitArray(length: Int, index: Int): ArrVisitor[Val, Val] = new ArrVisitor[Val, Val] {
    val a = new mutable.ArrayBuilder.ofRef[Lazy]
    def subVisitor: Visitor[_, _] = self
    def visitValue(v: Val, index: Int): Unit = a.+=(v)
    def visitEnd(index: Int): Val = new Val.Arr(pos, a.result())
  }

  def visitObject(length: Int, index: Int): ObjVisitor[Val, Val] = new ObjVisitor[Val, Val] {
    val cache = mutable.HashMap.empty[Any, Val]
    val allKeys = new util.LinkedHashMap[String, java.lang.Boolean]
    var key: String = null
    def subVisitor: Visitor[_, _] = self
    def visitKey(index: Int) = upickle.core.StringVisitor
    def visitKeyValue(s: Any): Unit = key = s.toString
    def visitValue(v: Val, index: Int): Unit = {
      cache.put(key, v)
      allKeys.put(key, false)
    }
    def visitEnd(index: Int): Val = new Val.Obj(pos, null, true, null, null, cache, allKeys)
  }

  def visitNull(index: Int): Val = Val.Null(pos)

  def visitFalse(index: Int): Val = Val.False(pos)

  def visitTrue(index: Int): Val = Val.True(pos)

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Val =
    Val.Num(pos,
      if (decIndex != -1 || expIndex != -1) s.toString.toDouble
      else upickle.core.ParseUtils.parseIntegralNum(s, decIndex, expIndex, index)
    )

  def visitString(s: CharSequence, index: Int): Val = Val.Str(pos, s.toString)

  override def visitJsonableObject(length: Int, index: Int): ObjVisitor[Val, Val] = new ObjVisitor[Val, Val] {
    val cache = mutable.HashMap.empty[Any, Val]
    val allKeys = new util.LinkedHashMap[String, java.lang.Boolean]
    var key: String = null

    def subVisitor: Visitor[_, _] = self

    def visitKey(index: Int) = upickle.core.StringVisitor

    def visitKeyValue(s: Any): Unit = key = s.toString

    def visitValue(v: Val, index: Int): Unit = {
      cache.put(key, v)
      allKeys.put(key, false)
    }

    def visitEnd(index: Int): Val = new Val.Obj(pos, null, true, null, null, cache, allKeys)
  }
}
