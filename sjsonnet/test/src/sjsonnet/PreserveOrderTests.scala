package sjsonnet

import utest._

object PreserveOrderTests extends TestSuite {
  def eval(s: String, preserveOrder: Boolean) = {
    new Interpreter(
      SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      DummyPath(),
      (_, _) => None,
      preserveOrder
    ).interpret(s, DummyPath("(memory)")) match {
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }

  def tests = Tests {
    test("preserveOrder") {

      eval(
        """{
             "z": "z",
             "a": "a",
           }""", true).toString() ==> """{"z":"z","a":"a"}"""
      eval(
        """{
             "z": "z",
             "a": "a",
           }""", false).toString() ==> """{"a":"a","z":"z"}"""

      eval(
        """{
             "z": "z",
             "a": "a",
           } + {
             "a": "a1",
             "z": "z1",
             "b": "b"
           }""", true).toString() ==> """{"z":"z1","a":"a1","b":"b"}"""
    }
  }
}