package org.bifrost.counterfeiter.tests

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class SimpleTest extends Spec with ShouldMatchers { 
  import org.bifrost.counterfeiter.{ExpressionParser, VariablePad, Expression, BasicFunctions, HtmlTemplateParser}
  import Expression.ElementaryExpression
  import scala.util.parsing.input.CharSequenceReader
  
  val variablePad = BasicFunctions.standardPad
  
  def testExpressionParser[T](str: String)(implicit m: Manifest[T]): T = 
    ExpressionParser.expression(new CharSequenceReader(str)) match { 
      case ExpressionParser.Success(res, next) => 
	res eval variablePad match {
	  case s: ElementaryExpression => s.extractOrThrow[T]
	  case other => fail(other.toString)
	}
      case o => fail(o.toString)
    }

  def testHtmlTemplateParser[T](in: String): String = 
    HtmlTemplateParser.htmlElem("")(new CharSequenceReader(in)) match {
      case HtmlTemplateParser.Success(res, next) => 
	res eval variablePad
      case o => fail(o.toString)
    }
	  
  it("test parse negative number") {
    testExpressionParser[String]("-123") should equal("-123")
  }

  it("test parse complex expression") {

    testExpressionParser[String]("123 + 123 / 123") should equal("124")
    testExpressionParser[String]("true and false xor true") should equal("true")
    testExpressionParser[String]("1 + 2 * 2 = 5 and true and 5 = 1 + 2 * 2 and 4 != 1 + 2 * 2") should equal ("true")
    testExpressionParser[String]("1 - 1 = 0") should equal ("true")    
    testExpressionParser[String]("-2 = 1 + (1 - 4)") should equal ("true")
    testExpressionParser[String]("4 = 1 - (1 - 4) * 1 * -1 * -1") should equal ("true")
  } 

  it("test parse string") {
    testExpressionParser[String](""" "Hello I am me" """) should equal ("Hello I am me")
    testExpressionParser[String](""" "Hello" = "Hello" """) should equal ("true")
    testExpressionParser[String](""" "Hello\"there" """) should equal ("Hello\"there")
  }
    
  it("test basic functions") { 
    testExpressionParser[String](""" substring("123", 1, 2) = "23" """) should equal ("true")
    testExpressionParser[String](""" substring(1 + 1, 0, 1) = "2" """) should equal ("true")
  }

  it("test lists") { 
    testExpressionParser[String](""" [1,2,3, "a"] """) should equal ("""[1, 2, 3, a]""")
  }

  it("test map") { 
    testExpressionParser[String](""" {1: 2, "abc": 5, 2 + 2: 9  } """) should equal (
      """{"1": 2, "abc": 5, "4": 9}""")
  }

  it("test primitive templates") {
    testHtmlTemplateParser(
"""#hello h1(style="display: none")
 | HI THERE
""") should equal ("""<div id="hello"><h1 style="display: none">HI THERE</h1></div>""")
  }

  it("test tag in tag") {
    testHtmlTemplateParser(
"""a
 b
  c
""") should equal ("<a><b><c/></b></a>")
  }

  it("test expression") { 
    testHtmlTemplateParser(
""".hello_class.t2 h1
 | Hi there { 1 + 2 }{1+2}{332+4}
 | Where am I?
""") should equal ("""<div class="hello_class t2"><h1>Hi there 33336
Where am I?</h1></div>""")
  }

  it("test if") {
   testHtmlTemplateParser(
"""#a
 + if 1 + 2 = 4
  h1
   | hello there
   span
    | go and die
   | hi there
 + else
  div
   | goodbye there
   span
    | go and die
   | hi there 
""") should equal ("""<div id="a"><div>goodbye there
<span>go and die</span>
hi there </div></div>""")
  }

  it("test for") {
    testHtmlTemplateParser(
"""+ for a in [1,2,3,"a",  5]
 b(id={a})
  | {a}
""") should equal ("""<b id="1">1</b><b id="2">2</b><b id="3">3</b><b id="a">a</b><b id="5">5</b>""") }
}
