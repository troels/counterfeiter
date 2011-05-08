package org.bifrost.counterfeiter

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
  
  class BasicExpression[T](value: T)(implicit manifest: Manifest[T]) extends ElementaryExpression {
    override def extract[S](implicit goalManifest: Manifest[S]): Option[S] = {
      if (goalManifest == manifest)
	Some(value.asInstanceOf[S])
      else if (goalManifest.erasure == classOf[String])
	Some(value.toString.asInstanceOf[S])
      else
	None
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
  
  class BinaryOperatorFunction[R, S, T](override val name: String, val priority: Int, f: (R, S) => T)
  extends FunctionExpression {
    override def numberOfArgs = 2
    override protected def execute(args: ElementaryExpression*)
                                  (implicit tman: Manifest[T], rman: Manifest[R], sman: Manifest[S]) = 
      new BasicExpression[T](f(args(0).extractOrThrow[R],args(1).extractOrThrow[S]))
  }

  class UnaryOperatorFunction[R, T](override val name: String, f: R => T) extends FunctionExpression {
    override def numberOfArgs = 1
    override protected def execute(args: ElementaryExpression*)
                                  (implicit tman: Manifest[T], rman: Manifest[R]) = 
      new BasicExpression[T](f(args(0).extractOrThrow[R]))
  }

  object MultiplyFunction extends BinaryOperatorFunction("*", 5, (a: Int, b: Int) =>  a * b)
  object PlusFunction extends BinaryOperatorFunction("+", 5, (a: Int, b: Int) => a + b)
  object MinusFunction extends BinaryOperatorFunction("-", 5, (a: Int, b: Int) => a - b)
  object DivideFunction extends BinaryOperatorFunction("/", 5, (a: Int, b: Int) => a / b)
  object ModulusFunction extends BinaryOperatorFunction("%", 5, (a: Int, b: Int) => a % b)
  object AndFunction extends BinaryOperatorFunction("and", 3, (a: Boolean, b: Boolean) => a && b)
  object OrFunction extends BinaryOperatorFunction("or", 2, (a: Boolean, b: Boolean) => a || b)
  object XorFunction extends BinaryOperatorFunction("xor", 2, (a: Boolean, b: Boolean) => a ^ b)
  object NotFunction extends UnaryOperatorFunction[Boolean, Boolean]("not", !_)
  object UnaryMinusFunction extends UnaryOperatorFunction[Int, Int]("-", -_)
  object EqualsFunction extends BinaryOperatorFunction[Any, Any, Boolean]("=", 1, _ == _)
  object NotEqualsFunction extends BinaryOperatorFunction[Any, Any, Boolean]("!=", 1, _ != _)

  val binaryOperators = List(MultiplyFunction, PlusFunction,   MinusFunction, 
			     DivideFunction,   AndFunction,    OrFunction, 
			     XorFunction,      EqualsFunction, NotEqualsFunction)
  val binaryOperatorMap = (binaryOperators map { op => op.name -> op })
                          .toMap[String, BinaryOperatorFunction[_, _, _]]
  
  val unaryOperators = List(NotFunction, UnaryMinusFunction)
  val unaryOperatorMap = (unaryOperators map { op => op.name -> op })
                         .toMap[String, UnaryOperatorFunction[_, _]]

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
 
