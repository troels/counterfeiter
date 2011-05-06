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
    def extract[T]: T 
  }
  
  class BasicExpression[T](value: T)(implicit manifest: ClassManifest[T]) extends ElementaryExpression {
    override def extract[S](implicit goalManifest: ClassManifest[S]): S = {
      if (goalManifest.erasure == manifest.erasure) {
	value.asInstanceOf[S]
      } else if (goalManifest.erasure == classOf[String]) {
	value.toString.asInstanceOf[S]
      } else {
	throw except("Failed to convert %s of type '%s' to '%s'",
		     value.toString, manifest.toString, goalManifest.toString)
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
    override def extract[S](implicit manifest: ClassManifest[S]) = 
      if (numberOfArgs == 0) 
	apply().extract[S]
      else
	throw except("Failed to convert %s to %s", name, manifest.erasure.toString)
  }
  
  class BinaryOperatorFunction[R, S, T](override val name: String, f: (R, S) => T) extends FunctionExpression {
    override def numberOfArgs = 2
    override protected def execute(args: ElementaryExpression*)(implicit manifest: ClassManifest[T]) = 
      new BasicExpression[T](f(args(0).extract[R],args(1).extract[S]))
  }

  class UnaryOperatorFunction[R, T](override val name: String, f: R => T) extends FunctionExpression {
    override def numberOfArgs = 1
    override protected def execute(args: ElementaryExpression*)(implicit manifest: ClassManifest[T]) = 
      new BasicExpression[T](f(args(0).extract[R]))
  }

  object MultiplyFunction extends BinaryOperatorFunction("*", ((_: Int) * (_: Int)))
  object PlusFunction extends BinaryOperatorFunction("+", ((_: Int) + (_: Int)))
  object MinusFunction extends BinaryOperatorFunction("-", ((_: Int) - (_: Int)))
  object DivideFunction extends BinaryOperatorFunction("/", ((_: Int) / (_: Int)))
  object AndFunction extends BinaryOperatorFunction("and", ((_: Boolean) && (_: Boolean)))
  object OrFunction extends BinaryOperatorFunction("or", ((_: Boolean) || (_: Boolean)))
  object XorFunction extends BinaryOperatorFunction("xor", ((_: Boolean) ^ (_: Boolean)))
  object NotFunction extends UnaryOperatorFunction[Boolean, Boolean]("not", !_)
  object UnaryMinusFunction extends UnaryOperatorFunction[Int, Int]("-", -_)
  object EqualsFunction extends BinaryOperatorFunction[Any, Any, Boolean]("=", (_ == _))

  abstract class ComplexExpression extends BaseExpression { 
    override def isElementary = false
  }
  
  class ApplicationExpression(func: FunctionExpression, args: BaseExpression*) extends ComplexExpression{
    override def eval(pad: Pad): ElementaryExpression = func(args map { _ eval pad } :_*)
  }
  
  class VariableExpression(variable: String) extends ComplexExpression { 
    override def eval(pad: Pad): ElementaryExpression = 
      pad lookup variable match {
	case Some(v) => v
	case None => throw except("%s is undefined" format variable)
      }
  }

  // class TemplateExpression(
}



