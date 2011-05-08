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
    Parser.expression(new CharSequenceReader("-123")) match { 
      case Parser.Success(res, next) => 
	res eval variablePad match {
	  case s: ElementaryExpression => s.extractOrThrow[T]
	  case other => fail(other.toString)
	}
      case o => fail(o.toString)
    }

  it("test parse negative number") {
    testExpressionParser[String]("-123") should equal("-123")
  }

  it("test parse addition expression") {
  } 
}
