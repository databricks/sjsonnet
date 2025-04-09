package sjsonnet

import utest._
object DecimalFormatTests extends TestSuite{
  def format(pattern: String, n: Double): String = {
    val (wholeStr, fracStrOpt, expStrOpt) = pattern.split("\\.", -1).map(_.split('E')) match{
      case Array(Array(wholeStr: String)) =>
        (wholeStr, None, None)
      case Array(Array(wholeStr: String, expStr: String)) =>
        (wholeStr, None, Some(expStr))
      case Array(Array(wholeStr: String), Array(fracStr: String)) =>
        (wholeStr, Some(fracStr), None)
      case Array(Array(wholeStr: String), Array(fracStr: String, expStr: String)) =>
        (wholeStr, Some(fracStr), Some(expStr))
    }
    val wholeLength = wholeStr.length
    assert(wholeLength == 1)
    val fracLengthOpt = fracStrOpt.map(fracStr => (fracStr.count(_ == '0'), fracStr.count(_ == '#')))
    val expLengthOpt = expStrOpt.map(_.length)
    sjsonnet.DecimalFormat.format(fracLengthOpt, expLengthOpt, n)
  }
  def tests: Tests = Tests{

    test - {format("0.000000E00",910) ==> "9.100000E02"}
    test - {format("0E00",910) ==> "9E02"}
    test - {format("0E00",0.009123) ==> "9E-03"}
    test - {format("0E0",0.009123) ==> "9E-3"}
    test - {format("0E0",0.000000000009123) ==> "9E-12"}
    test - {format("0.000000E00",-910) ==> "-9.100000E02"}
    test - {format("0.0000E00",910.3) ==> "9.1030E02"}
    test - {format("0.E00",910.3) ==> "9.E02"}
    test - {format("0.E00",900) ==> "9.E02"}
    test - {format("0.000E00",1000000001) ==> "1.000E09"}
    test - {format("0.000000",910) ==> "910.000000"}
    test - {format("0.000000",0) ==> "0.000000"}
    test - {format("0",910) ==> "910"}
    test - {format("0.000000",-910) ==> "-910.000000"}
    test - {format("0.0000",910.3) ==> "910.3000"}
    test - {format("0.",910.3) ==> "910."}
    test - {format("0.",910) ==> "910."}
    test - {format("0.000",1000000001) ==> "1000000001.000"}
    test - {format("0.00E00",1000000001) ==> "1.00E09"}
    test - {format("0.00E00",1100) ==> "1.10E03"}
    test - {format("0.00",1.1) ==> "1.10"}
    test - {format("0.0000E00",1000000001) ==> "1.0000E09"}
//    test - {format("0.0",1100) ==> "1100.0"}
//    test - {format("0.00",110) ==> "110.00"}
    test - {format("0.0000",1.1) ==> "1.1000"}
    test - {format("0.##E00",1000000001) ==> "1E09"}
    test - {format("0.##E00",1100) ==> "1.1E03"}
    test - {format("0.##",1.1) ==> "1.1"}
    test - {format("0.####E00",1000000001) ==> "1E09"}
//    test - {format("0000.#",1100) ==> "1100"}
//    test - {format("000.##",110) ==> "110"}
    test - {format("0.####",1.1) ==> "1.1"}
//    test - {format("0.000E00", 123456789) ==> "1E+0800"}
//    test - {format("0E0000", 123) ==> "1.230000E+020000"}
  }
}
