package sjsonnet

import java.util

import ujson.JsVisitor
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable

/** Parse JSON directly into a literal `Val` */
class ValVisitor(pos: Position) extends JsVisitor[Val, Val] { self =>

  override def visitJsonableObject(length: Int, index: Int): ObjVisitor[Val, Val] =
    visitObject(length, index)

  def visitArray(length: Int, index: Int): ArrVisitor[Val, Val] = new ArrVisitor[Val, Val] {
    val a = new mutable.ArrayBuilder.ofRef[Eval]
    if (length >= 0) a.sizeHint(length)
    def subVisitor: Visitor[?, ?] = self
    def visitValue(v: Val, index: Int): Unit = a.+=(v)
    def visitEnd(index: Int): Val = Val.Arr(pos, a.result())
  }

  def visitObject(length: Int, index: Int): ObjVisitor[Val, Val] = new ObjVisitor[Val, Val] {
    val cache = new java.util.HashMap[Any, Val]()
    val allKeys = new util.LinkedHashMap[String, java.lang.Boolean]
    var key: String = _
    def subVisitor: Visitor[?, ?] = self
    def visitKey(index: Int): upickle.core.StringVisitor.type = upickle.core.StringVisitor
    def visitKeyValue(s: Any): Unit = key = ValVisitor.replaceLoneSurrogates(s.toString)
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
    Val.Num(
      pos,
      if (decIndex != -1 || expIndex != -1) s.toString.toDouble
      else if (s.length() == 2 && s.charAt(0) == '-' && s.charAt(1) == '0') -0.0
      else {
        try upickle.core.ParseUtils.parseIntegralNum(s, decIndex, expIndex, index).toDouble
        catch { case _: NumberFormatException => s.toString.toDouble }
      }
    )

  def visitString(s: CharSequence, index: Int): Val =
    Val.Str(pos, ValVisitor.replaceLoneSurrogates(s.toString))
}

object ValVisitor {

  /**
   * Replace unpaired UTF-16 surrogates with U+FFFD. JSON inputs may contain lone
   * surrogate escapes (RFC 8259 lets implementations accept them); keeping them
   * would corrupt to '?' on UTF-8 output. Matches the replacement policy of
   * std.char / %c and go-jsonnet's JSON decoding. Strings without surrogates are
   * returned as-is without allocation.
   */
  private[sjsonnet] def replaceLoneSurrogates(s: String): String = {
    val len = s.length
    var i = 0
    while (i < len) {
      val c = s.charAt(i)
      if (c >= 0xd800 && c <= 0xdfff) {
        val sb = new java.lang.StringBuilder(len)
        sb.append(s, 0, i)
        while (i < len) {
          val ch = s.charAt(i)
          if (
            Character.isHighSurrogate(ch) && i + 1 < len &&
            Character.isLowSurrogate(s.charAt(i + 1))
          ) {
            sb.append(ch)
            sb.append(s.charAt(i + 1))
            i += 2
          } else if (ch >= 0xd800 && ch <= 0xdfff) {
            sb.append('\ufffd')
            i += 1
          } else {
            sb.append(ch)
            i += 1
          }
        }
        return sb.toString
      }
      i += 1
    }
    s
  }
}
