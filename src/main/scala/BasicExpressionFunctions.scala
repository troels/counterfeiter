package org.bifrost.counterfeiter
import scala.math

object BasicFunctions {
  import Expression.{FunctionExpression, ElementaryExpression, BasicExpression}
  
  case class StandardFunctionExpression(
    name: String, numberOfArgs: Int, f: (ElementaryExpression*) => ElementaryExpression) 
  extends FunctionExpression {
    override protected def execute(args: ElementaryExpression*) = f(args :_*)
  }

  def argumentDeconstructor[R, S, T](f: (R, S) => T)(implicit mr: Manifest[R], ms: Manifest[S], mt: Manifest[T]) = 
    {
      def g(args: ElementaryExpression*) = f(args(0).extractOrThrow[R], args(1).extractOrThrow[S])
      g _ 
    }

  def argumentDeconstructor[R, S, T, U](f: (R, S, T) => U)
                                     (implicit mr: Manifest[R], ms: Manifest[S], mt: Manifest[T], mu: Manifest[U]):
    (ElementaryExpression*) => U =
  {
    def g(args:  ElementaryExpression*) = f(
      args(0).extractOrThrow[R], args(1).extractOrThrow[S], args(2).extractOrThrow[T])
    g
  }

  
  def substring = StandardFunctionExpression(
    "substring", 3, argumentDeconstructor[String, Int, Int, ElementaryExpression] { 
      (str, _start, len) => {
	val strLen = str.length
	val start = math.min(math.max(_start, 0), strLen)
	val end = math.min(start + len, strLen)
	new BasicExpression[String](str substring (start, end))
      }
    } )
					     
  
  val functionList = List(substring)
  
  val standardPad = new VariablePad(functionList map { f => f.name -> f } toMap)
}
