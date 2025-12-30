package sjsonnet.starlark

import com.oracle.truffle.api._
import com.oracle.truffle.api.frame.VirtualFrame
import com.oracle.truffle.api.nodes.RootNode
import com.oracle.truffle.api.TruffleLanguage
import com.oracle.truffle.api.TruffleLanguage.Registration

@Registration(id = "tiny", name = "Tiny Language", version = "1.0")
class TinyLanguage extends TruffleLanguage[Context] {
  override def createContext(env: TruffleLanguage.Env): Context = new Context()

  override def parse(request: TruffleLanguage.ParsingRequest): CallTarget = {
    val limit = request.getSource.getCharacters.toString.trim.toInt
    val root = new TinyRootNode(this, limit)
    root.getCallTarget
  }
}

class Context()

class TinyRootNode(language: TinyLanguage, limit: Int) extends RootNode(language) {
  override def execute(frame: VirtualFrame): AnyRef = {
    var i = 0
    while (i < limit) {
      i += 1
    }
    java.lang.Integer.valueOf(i)
  }
}
