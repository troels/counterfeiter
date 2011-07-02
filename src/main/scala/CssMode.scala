package org.bifrost.counterfeiter

import scala.util.parsing.combinator.{ Parsers, RegexParsers, ImplicitConversions }
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.CharSequenceReader.EofCh


object CssStructs { 
  case class CssRule(queryString: String, rules: List[(String, String)], subrules: List[CssRule])
}

object CssMode extends RegexParsers with ImplicitConversions { 
  def cssRule(indent: String): Parser[String] = ""
}
