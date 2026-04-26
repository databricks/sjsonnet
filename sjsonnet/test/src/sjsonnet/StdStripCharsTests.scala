package sjsonnet

import sjsonnet.TestUtils.{eval, evalErr}
import utest._
object StdStripCharsTests extends TestSuite {
  private val party = new String(Array(0xd83c.toChar, 0xdf89.toChar))
  private val partyEsc = "\\" + "uD83C" + "\\" + "uDF89"
  private val eAcuteEsc = "\\" + "u00e9"

  def tests: Tests = Tests {
    test("stdRStripChars") {
      eval("std.rstripChars(\" test test test \", \" \")").toString() ==> """" test test test""""
      eval("std.rstripChars(\"aaabbbbcccc\", \"ac\")").toString() ==> """"aaabbbb""""
      eval("std.rstripChars(\"cacabbbbaacc\", \"ac\")").toString() ==> """"cacabbbb""""
      eval("std.rstripChars(\"cacabbcacabbaacc\", \"ac\")").toString() ==> """"cacabbcacabb""""
      eval("std.rstripChars(\"cacabbcacabb-aacc\", \"a-c\")").toString() ==> """"cacabbcacabb""""

      eval("""std.rstripChars("cacabbcacabb[aacc]", "ac[]$%^&*(")""")
        .toString() ==> """"cacabbcacabb""""
    }
    test("stdLStripChars") {
      eval("std.lstripChars(\" test test test \", \" \")").toString() ==> """"test test test """"
      eval("std.lstripChars(\"aaabbbbcccc\", \"ac\")").toString() ==> """"bbbbcccc""""
      eval("std.lstripChars(\"cacabbcacabbaacc\", \"ac\")").toString() ==> """"bbcacabbaacc""""
      eval("std.lstripChars(\"-cacabbcacabbaacc\", \"a-c\")").toString() ==> """"bbcacabbaacc""""

      eval("std.lstripChars(\"[]aaabbbbcccc\", \"[ac]\")").toString() ==> """"bbbbcccc""""

    }
    test("stdStripChars") {
      eval("std.stripChars(\" test test test \", \" \")").toString() ==> """"test test test""""
      eval("std.stripChars(\"aaabbbbcccc\", \"ac\")").toString() ==> """"bbbb""""
      eval("std.stripChars(\"cacabbcacabbaacc\", \"ac\")").toString() ==> """"bbcacabb""""
      eval("std.stripChars(\"c-acabbca-cabbaacc-\", \"a-c\")").toString() ==> """"bbca-cabb""""

      eval("std.stripChars(\"[aaabbbbcccc]\", \"ac[]\")").toString() ==> """"bbbb""""
    }
    test("asciiStripCharsKeepsNonAsciiCodepoints") {
      eval(s"""std.stripChars("--${partyEsc}hello${partyEsc}--", "-")""").str ==> s"${party}hello${party}"
      eval(s"""std.lstripChars("--${partyEsc}hello", "-")""").str ==> s"${party}hello"
      eval(s"""std.rstripChars("hello${partyEsc}--", "-")""").str ==> s"hello${party}"
    }
    test("stripCharsFallsBackForNonAsciiStripSet") {
      eval(s"""std.stripChars("${partyEsc}-hello-${partyEsc}", "${partyEsc}-")""").str ==> "hello"
      eval(s"""std.lstripChars("${partyEsc}-hello-${partyEsc}", "${partyEsc}-")""")
        .str ==> s"hello-${party}"
      eval(s"""std.rstripChars("${partyEsc}-hello-${partyEsc}", "${partyEsc}-")""")
        .str ==> s"${party}-hello"
      eval(s"""std.stripChars("${eAcuteEsc}-hello-${eAcuteEsc}", "${eAcuteEsc}-")""").str ==> "hello"
    }
    test("asciiStripCharsHandlesBitsetBoundaries") {
      eval("std.stripChars(\"?@hello\\u007f\", \"?@\\u007f\")").toString() ==> """"hello""""
      eval("std.stripChars(\"\\u001f_hello_\\u007f\", \"\\u001f_\\u007f\")").toString() ==> """"hello""""
      eval("std.stripChars(\"\", \"?@\\u007f\")").toString() ==> """"""""
      eval("""std.stripChars("hello", "")""").toString() ==> """"hello""""
    }
    test("stripCharsForcesCharsBeforeStr") {
      assert(
        evalErr("""std.stripChars(error "str first", error "chars first")""").startsWith(
          "sjsonnet.Error: chars first"
        )
      )
      assert(
        evalErr("""std.lstripChars(error "str first", error "chars first")""").startsWith(
          "sjsonnet.Error: chars first"
        )
      )
      assert(
        evalErr("""std.rstripChars(error "str first", error "chars first")""").startsWith(
          "sjsonnet.Error: chars first"
        )
      )
    }

  }
}
