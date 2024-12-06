package sjsonnet

import utest._
import TestUtils.eval
object StdStripCharsTests extends TestSuite {

  def tests = Tests {
    test("stdRStripChars") {
      eval("std.rstripChars(\" test test test \", \" \")").toString() ==> """" test test test""""
      eval("std.rstripChars(\"aaabbbbcccc\", \"ac\")").toString() ==> """"aaabbbb""""
      eval("std.rstripChars(\"cacabbbbaacc\", \"ac\")").toString() ==> """"cacabbbb""""
      eval("std.rstripChars(\"cacabbcacabbaacc\", \"ac\")").toString() ==> """"cacabbcacabb""""

      eval("""std.rstripChars("cacabbcacabb[aacc]", "ac[]$%^&*(")""").toString() ==> """"cacabbcacabb""""
    }
    test("stdLStripChars") {
      eval("std.lstripChars(\" test test test \", \" \")").toString() ==> """"test test test """"
      eval("std.lstripChars(\"aaabbbbcccc\", \"ac\")").toString() ==> """"bbbbcccc""""
      eval("std.lstripChars(\"cacabbcacabbaacc\", \"ac\")").toString() ==> """"bbcacabbaacc""""

      eval("std.lstripChars(\"[]aaabbbbcccc\", \"[ac]\")").toString() ==> """"bbbbcccc""""

    }
    test("stdStripChars") {
      eval("std.stripChars(\" test test test \", \" \")").toString() ==> """"test test test""""
      eval("std.stripChars(\"aaabbbbcccc\", \"ac\")").toString() ==> """"bbbb""""
      eval("std.stripChars(\"cacabbcacabbaacc\", \"ac\")").toString() ==> """"bbcacabb""""

      eval("std.stripChars(\"[aaabbbbcccc]\", \"ac[]\")").toString() ==> """"bbbb""""
    }

  }
}
