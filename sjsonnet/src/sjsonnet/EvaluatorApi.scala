package sjsonnet

class EvaluatorApi(val extVars: Map[String, ujson.Value], val wd: Path){
  def memoryScope = new ScopeApi(wd / "(memory)", wd)
}
