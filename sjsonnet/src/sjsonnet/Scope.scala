package sjsonnet

case class FileScope(currentFile: Path,
                     currentRoot: Path,
                     nameIndices: Map[String, Int])
object Scope{


  def empty() = {
    new Scope(None, None, None, Array.empty, None)
  }

  def standard() = new Scope(
    None, None, None, Array(Lazy(Std.Std)), None
  )
}

case class Scope(dollar0: Option[Val.Obj],
                 self0: Option[Val.Obj],
                 super0: Option[Val.Obj],
                 bindings0: Array[Lazy],
                 delegate: Option[Scope]) {
  def dollar = dollar0.get
  def self = self0.get
  val bindingCache = collection.mutable.Map.empty[Int, Option[Lazy]]
  def bindings(k: Int): Option[Lazy] = bindingCache.getOrElseUpdate(
    k,
    bindings0.lift(k).orElse(delegate.flatMap(_.bindings(k)))
  )
  def ++(traversableOnce: TraversableOnce[(Int, (Val.Obj, Option[Val.Obj]) => Lazy)]) = {
    val newBindings = java.util.Arrays.copyOf(bindings0, bindings0.length)
    for((i, v) <- traversableOnce) newBindings(i) = v.apply(self0.getOrElse(null), super0)
    new Scope(
      dollar0,
      self0,
      super0,
      newBindings,
      Some(this),
    )
  }
}