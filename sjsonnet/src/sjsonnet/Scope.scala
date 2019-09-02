package sjsonnet


object Scope{


  def empty(root: Path) = new Scope(None, None, None, Map.empty,root / "(memory)", root,  None)
  def standard(p: Path,
               currentRoot: Path) = new Scope(
    None, None, None, Map("std" -> Lazy(Std.Std)), p, currentRoot, None
  )
}

class ScopeApi(val currentFile: Path, val currentRoot: Path)
case class Scope(dollar0: Option[Val.Obj],
                 self0: Option[Val.Obj],
                 super0: Option[Val.Obj],
                 bindings0: Map[String, Lazy],
                 override val currentFile: Path,
                 override val currentRoot: Path,
                 delegate: Option[Scope]) extends ScopeApi(currentFile, currentRoot){
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
      currentFile,
      currentRoot,
      Some(this)
    )
  }
}