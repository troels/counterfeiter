package org.bifrost.counterfeiter

import scala.reflect.AnyValManifest

object Expression {
  class ExpressionEvaluationException(msg: String) extends U.CounterFeiterException(msg)
  
  def except(format: String, args: Any*) = 
    new ExpressionEvaluationException(format.format(args :_*))

  abstract sealed class BaseExpression {
    def isElementary: Boolean
    def eval(pad: Pad): ElementaryExpression
  }
  
  abstract sealed class ElementaryExpression extends BaseExpression {
    override def isElementary = true
    override def eval(pad: Pad): ElementaryExpression = this
    def extract[T](implicit manifest: Manifest[T]): Option[T] 
    
    def extractOrThrow[T](implicit manifest: Manifest[T]): T = 
      extract[T] match { 
	case Some(v) => v
	case None => throw except(
	  "Failed to extract type '%s' from %s", manifest.toString, toString)
      }
  }
  
  class BasicExpression[T](value: T)(implicit m: Manifest[T]) extends ElementaryExpression {
    override def extract[S](implicit goalManifest: Manifest[S]): Option[S] = {
      if (goalManifest == manifest[String]) {
	Some(value.toString.asInstanceOf[S])
      } else if (m.isInstanceOf[AnyValManifest[_]]) {
	if (m == goalManifest) {
	  Some(value.asInstanceOf[S])
	} else {
	  None
	}
      } else if (value.isInstanceOf[S]) {
	Some(value.asInstanceOf[S])
      } else {
	None
      }
    }
  }
  
  class CurriedFunctionExpression(fn: FunctionExpression, args: ElementaryExpression*) extends FunctionExpression { 
    assert (fn.numberOfArgs >= args.size)

    def name = "%s<%d args>" format(fn.name, args.size)
    def numberOfArgs: Int = fn.numberOfArgs - args.size
    
    override protected def execute(args: ElementaryExpression*) =
      fn((this.args ++ args) :_*)
  }
  
  abstract class FunctionExpression extends ElementaryExpression {
    def name: String
    def numberOfArgs: Int
    protected def execute(args: ElementaryExpression*): ElementaryExpression

    def apply(args: ElementaryExpression*): ElementaryExpression = 
      if (args.length > numberOfArgs) {
	throw except("Too many arguments to function %s; expected: %d, got %d",
		     toString, numberOfArgs, args.length)
      } else if (args.length < numberOfArgs) {
	new CurriedFunctionExpression(this, args: _*)
      } else  {
	execute(args :_*)
      }
    
    override def toString = "Function: %s".format(name)
    override def extract[S](implicit manifest: Manifest[S]) = 
      if (numberOfArgs == 0) 
	apply().extract[S]
      else
	throw except("Failed to convert %s to %s", name, manifest.erasure.toString)
  }
  
  abstract class BinaryOperatorFunction(override val name: String, val priority: Int)
  extends FunctionExpression {
    override def numberOfArgs = 2
  }

  abstract class UnaryOperatorFunction(override val name: String) extends FunctionExpression {
    override def numberOfArgs = 1
  }

  object MultiplyFunction extends BinaryOperatorFunction("*", 5) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Int] * args(1).extractOrThrow[Int])
  }
  object PlusFunction extends BinaryOperatorFunction("+", 5) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Int] + args(1).extractOrThrow[Int])
  }
  object MinusFunction extends BinaryOperatorFunction("-", 5) { 
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Int] - args(1).extractOrThrow[Int])
  }
  object DivideFunction extends BinaryOperatorFunction("/", 5) { 
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Int] / args(1).extractOrThrow[Int])
  }

  object ModulusFunction extends BinaryOperatorFunction("%", 5) { 
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Int] % args(1).extractOrThrow[Int])
  }
  object AndFunction extends BinaryOperatorFunction("and", 3) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Boolean] && args(1).extractOrThrow[Boolean])
  }
  object OrFunction extends BinaryOperatorFunction("or", 2)  {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Boolean] || args(1).extractOrThrow[Boolean])
  }
  object XorFunction extends BinaryOperatorFunction("xor", 2) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Boolean] ^ args(1).extractOrThrow[Boolean])
  }

  object EqualsFunction extends BinaryOperatorFunction("=", 1) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Any] == args(1).extractOrThrow[Any])
  }
  object NotEqualsFunction extends BinaryOperatorFunction("!=", 1) {
     override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Any] != args(1).extractOrThrow[Any])
  }

  object NotFunction extends UnaryOperatorFunction("not") {
     override protected def execute(args: ElementaryExpression*) =
       new BasicExpression(!args(0).extractOrThrow[Boolean])
  }

  object UnaryMinusFunction extends UnaryOperatorFunction("-") {
     override protected def execute(args: ElementaryExpression*) =
       new BasicExpression(-args(0).extractOrThrow[Int])
  }

  val binaryOperators = List(MultiplyFunction, PlusFunction,   MinusFunction, 
			     DivideFunction,   AndFunction,    OrFunction, 
			     XorFunction,      EqualsFunction, NotEqualsFunction)
  val binaryOperatorMap = (binaryOperators map { op => op.name -> op })
                          .toMap[String, BinaryOperatorFunction]
  
  val unaryOperators = List(NotFunction, UnaryMinusFunction)
  val unaryOperatorMap = (unaryOperators map { op => op.name -> op })
                         .toMap[String, UnaryOperatorFunction]

  abstract class ComplexExpression extends BaseExpression { 
    override def isElementary = false
  }
  
  class ApplicationExpression(func: BaseExpression, args: BaseExpression*) extends ComplexExpression{
    override def eval(pad: Pad): ElementaryExpression = {
      func eval pad match {
	case f: FunctionExpression => f(args map { _ eval pad } :_*)
	case g => throw except("Required function expression, got: %s", g.toString)
      }
    }
  }
  
  class VariableExpression(variable: String) extends ComplexExpression { 
    override def eval(pad: Pad): ElementaryExpression = 
      pad lookup variable match {
	case Some(v) => v
	case None => throw except("%s is undefined" format variable)
      }
  }

}
 
