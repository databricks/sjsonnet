package sjsonnet

import org.graalvm.polyglot._

object PythonExample {
  def main(args: Array[String]): Unit = {
    println("Initializing GraalVM Polyglot Context for Python...")
    try {
      val context = Context.newBuilder("python")
        .option("engine.WarnInterpreterOnly", "false") 
        .build()
      
      println("Context created.")
      
      val source = "1 + 2"
      println(s"Evaluating: $source")
      
      val result = context.eval("python", source)
      
      println(s"Result: ${result.asInt()}")
      
      val pyFunc = 
        """
          |def greet(name):
          |    return "Hello, " + name
          |""".stripMargin
      
      context.eval("python", pyFunc)
      val greetFunc = context.getPolyglotBindings.getMember("greet") 
      // Note: In Python, top-level functions are often in the polyglot bindings or main module.
      // Let's access the main module.
      val mainModule = context.getBindings("python")
      val greet = mainModule.getMember("greet")
      
      if (greet != null && greet.canExecute()) {
        val greeting = greet.execute("World")
        println(s"Function call result: ${greeting.asString()}")
      } else {
         println("Could not find 'greet' function.")
      }
      
      context.close()
    } catch {
      case e: Exception => 
        e.printStackTrace()
    }
  }
}
