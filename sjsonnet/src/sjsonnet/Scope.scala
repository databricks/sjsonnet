package sjsonnet


import ammonite.ops.Path

object Scope{


  def empty = new Scope(None, None, None, Map.empty, ammonite.ops.pwd / "(memory)", List(), None)
  def standard(p: Path, s: List[Path]) = new Scope(None, None, None, Map("std" -> Lazy(Std.Std)), p, s, None)
}

case class Scope(dollar0: Option[Val.Obj],
                 self0: Option[Val.Obj],
                 super0: Option[Val.Obj],
                 bindings0: Map[String, Lazy],
                 fileName: Path,
                 searchRoots: List[Path],
                 delegate: Option[Scope]){
  def dollar = dollar0.get
  def self = self0.get
  val bindingCache = collection.mutable.Map.empty[String, Option[Lazy]]
  def bindings(k: String): Option[Lazy] = bindingCache.getOrElseUpdate(
    k,
    bindings0.get(k).orElse(delegate.flatMap(_.bindings(k)))
  )
  def ++(traversableOnce: TraversableOnce[(String, (Val.Obj, Option[Val.Obj]) => Lazy)]) = {
    new Scope(
      dollar0,
      self0,
      super0,
      traversableOnce.map{case (k, v) => (k, v.apply(self0.getOrElse(null), super0))}.toMap,
      fileName,
      searchRoots,
      Some(this)
    )
  }
}