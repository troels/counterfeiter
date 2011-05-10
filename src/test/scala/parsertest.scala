package org.bifrost.counterfeiter.tests

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class SimpleTest extends Spec with ShouldMatchers { 
  import org.bifrost.counterfeiter.{Parser, VariablePad, Expression}
  import Expression.ElementaryExpression
  import scala.util.parsing.input.CharSequenceReader
  
  val variablePad = new VariablePad()
  
  def testExpressionParser[T](str: String)(implicit m: Manifest[T]): T = 
    Parser.expression(new CharSequenceReader(str)) match { 
      case Parser.Success(res, next) => 
	res eval variablePad match {
	  case s: ElementaryExpression => s.extractOrThrow[T]
	  case other => fail(other.toString)
	}
      case o => fail(o.toString)
    }

  it("test parse negative number") {
    // testExpressionParser[String]("-123") should equal("-123")
  }

  it("test parse complex expression") {

    testExpressionParser[String]("123 + 123 / 123") should equal("124")
    testExpressionParser[String]("true and false xor true") should equal("true")
    testExpressionParser[String]("1 + 2 * 2 = 5 and true and 5 = 1 + 2 * 2 and 4 != 1 + 2 * 2") should equal ("true")
    testExpressionParser[String]("1 - 1 = 0") should equal ("true")    
    testExpressionParser[String]("-2 = 1 + (1 - 4)") should equal ("true")
    testExpressionParser[String]("4 = 1 - (1 - 4) * 1 * -1 * -1") should equal ("true")
  } 
}
