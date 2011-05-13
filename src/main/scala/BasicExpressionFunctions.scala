package org.bifrost.counterfeiter
import scala.math

object BasicFunctions {
  import Expression.{FunctionExpression, ElementaryExpression, BasicExpression}
  
  case class StandardFunctionExpression(name: String, numberOfArgs: Int, 
					f: (ElementaryExpression*) => ElementaryExpression) 
  extends FunctionExpression {
    override protected def execute(args: ElementaryExpression*) = f(args :_*)
  }


  def substringF(args: ElementaryExpression*): ElementaryExpression = {
    val str = args(0).extractOrThrow[String]
    val strLen = str.length
    val start = math.min(math.max(args(1).extractOrThrow[Int], 0), strLen)
    val len = args(2).extractOrThrow[Int]
    val end = math.min(start + len, strLen)
    new BasicExpression[String](str substring(start, end))
  }
  def substring = StandardFunctionExpression("substring", 3, substringF _)
  
  val functionList = List(substring)
  
  val standardPad = new VariablePad(functionList map { f => f.name -> f } toMap)
}
