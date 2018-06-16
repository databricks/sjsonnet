package sjsonnet

import utest._
object Format{
  import fastparse.all._
  val identifier       = P( CharsWhileIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "_") )
  val integer           = P( CharsWhileIn('0' to '9') )
  val replacement_field = P( "{" ~ field_name.? ~ ("!" ~ conversion).? ~ (":" ~ format_spec).? ~ "}" )
  val field_name        = P( arg_name ~ ("." ~ attribute_name | "[" ~ element_index ~ "]").rep )
  val arg_name          = P( (identifier | integer).? )
  val attribute_name    = P( identifier )
  val element_index     = P( integer | index_string )
  val index_string      = P( CharsWhile(_ != ']') )
  val conversion        = P( "r" | "s" | "a" )

  val format_spec = P( (fill.? ~ align).? ~ sign.? ~ "#".? ~ "0".? ~ width.? ~ ",".? ~ ("." ~ precision).? ~ `type`.? )
  val fill        = P( AnyChar )
  val align       = P( "<" | ">" | "=" | "^" )
  val sign        = P( "+" | "-" | " " )
  val width       = P( integer )
  val precision   = P( integer )
  val `type`      = P( "b" | "c" | "d" | "e" | "E" | "f" | "F" | "g" | "G" | "n" | "o" | "s" | "x" | "X" | "%" )
}
object FormatTests extends TestSuite{

  def tests = Tests{
    ""
  }
}

