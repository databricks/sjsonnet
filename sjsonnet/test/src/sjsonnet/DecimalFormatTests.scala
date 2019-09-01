package sjsonnet

import utest._
import sjsonnet.DecimalFormat.format
object DecimalFormatTests extends TestSuite{

  def tests = Tests{

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
    test - {format("0000.0",1100) ==> "1100.0"}
    test - {format("000.00",110) ==> "110.00"}
    test - {format("0.0000",1.1) ==> "1.1000"}
    test - {format("0.##E00",1000000001) ==> "1E09"}
    test - {format("0.##E00",1100) ==> "1.1E03"}
    test - {format("0.##",1.1) ==> "1.1"}
    test - {format("0.####E00",1000000001) ==> "1E09"}
    test - {format("0000.#",1100) ==> "1100"}
    test - {format("000.##",110) ==> "110"}
    test - {format("0.####",1.1) ==> "1.1"}
    test - {format("0000.000E00", 123456789) ==> "1234.568E05"}
    test - {format("0000E0000", 123) ==> "1230E-0001"}
  }
}
