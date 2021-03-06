package org.bifrost.counterfeiter

import scala.reflect.AnyValManifest
import scala.collection.JavaConversions._
import U.Implicits._

object HtmlEscapedString { 
  def apply(str:  String) = new HtmlEscapedString(str)
  def escape(str: String) = new HtmlEscapedString(U.escapeHtml(str))
}

class HtmlEscapedString(str: String) {
  override def toString = str
  
  override def equals(obj: Any): Boolean =
    return obj.isInstanceOf[HtmlEscapedString] && obj.asInstanceOf[HtmlEscapedString].toString == toString
}

object Expression {
  class ExpressionEvaluationException(msg: String) extends U.CounterFeiterException(msg)

  def getStringEscaped(arg: ElementaryExpression): String =  
    (arg.extract[HtmlEscapedString] getOrElse HtmlEscapedString.escape(arg.extractOrThrow[String])).toString
  
  
  def except(format: String, args: Any*) = 
    new ExpressionEvaluationException(format.format(args :_*))

  abstract sealed class BaseExpression {
    def isElementary: Boolean
    def eval(m: Machine): ElementaryExpression
  }
  
  abstract sealed class ElementaryExpression extends BaseExpression {
    override def isElementary = true
    override def eval(m: Machine): ElementaryExpression = this
    def extract[T](implicit manifest: Manifest[T]): Option[T] 
    
    def extractOrThrow[T](implicit manifest: Manifest[T]): T = 
      extract[T] match { 
        case Some(v) => v
        case None => throw except(
          "Failed to extract type '%s' from %s", manifest.toString, toString)
      }
  }


  // UntypedExpression / BasicExpression's respective responsibilities need to be clarified 
  // or one of them removed.
  class UntypedExpression(val value: AnyRef) extends ElementaryExpression {
    override def equals(that: Any) = {
      that match {
        case _that: UntypedExpression => _that.value == value
        case _that: BasicExpression[_] => _that.value == value
        case _ => false
      }
    }

    override def extract[S](implicit ms: Manifest[S]): Option[S] =  {
      if (ms == manifest[Boolean] || ms == manifest[java.lang.Boolean]) {
        if (value.isInstanceOf[java.lang.Boolean]) {
          Some(value.asInstanceOf[S])
        } else if (value.isInstanceOf[Option[_]]) { 
          if (value == None) { 
            Some(java.lang.Boolean.FALSE.asInstanceOf[S])
          } else {
            Some(java.lang.Boolean.TRUE.asInstanceOf[S])
          }
        } else if (value.isInstanceOf[Seq[_]]) { 
          Some((!value.asInstanceOf[Seq[_]].isEmpty).asInstanceOf[S])
        } else {
          if (value == null) {
            Some(java.lang.Boolean.FALSE.asInstanceOf[S])
          } else {
            Some(java.lang.Boolean.TRUE.asInstanceOf[S])
          }
        }
      } else if (ms.erasure == classOf[List[_]]) { 
        if (value.isInstanceOf[java.util.List[_]]) {
          Some(((value.asInstanceOf[java.util.List[AnyRef]] toList) map {
            v => new UntypedExpression(v) } toList).asInstanceOf[S])
        } else if (value.isInstanceOf[List[_]]) {
          Some((value.asInstanceOf[List[AnyRef]] map { v => new UntypedExpression(v) }).asInstanceOf[S])
        } else if (value.isInstanceOf[Array[_]]) { 
          Some((value.asInstanceOf[Array[AnyRef]] map { v => new UntypedExpression(v) } toList).asInstanceOf[S])
        } else if (value == null) { 
          Some(List().asInstanceOf[S])
        } else {
          None
        }
      } else if (ms.erasure == classOf[Map[_, _]] && value == null) { 
        Some(Map().asInstanceOf[S])
      } else if (ms.erasure == manifest[Map[_, _]].erasure && value.isInstanceOf[Map[_, _]]) {
        Some((value.asInstanceOf[Map[AnyRef, AnyRef]] map { 
          case (k, v) => k.toString -> new UntypedExpression(v) 
        }).asInstanceOf[S])
      } else if (ms.erasure.isInstance(value)) {
        Some(value.asInstanceOf[S])
      } else if (ms == manifest[String]) { 
        Some(if (value == null) "null".asInstanceOf[S] else value.toString.asInstanceOf[S])
      } else if ((ms == manifest[Int] || ms == manifest[java.lang.Integer])  && 
                 value.isInstanceOf[java.lang.Integer]) {
        Some(value.asInstanceOf[S])
      } else {
         None
      }
    }
  }

    
  class BasicExpression[T](val value: T)(implicit m: Manifest[T]) extends ElementaryExpression {
    override def equals(that: Any) = {
      that match { 
        case _that: UntypedExpression => _that.value == value
        case _that: BasicExpression[_] => _that.value == value
        case _ => false
      }
    }

    override def extract[S](implicit goalManifest: Manifest[S]): Option[S] = {
      if (goalManifest == manifest[String]) {
        Some(value.toString.asInstanceOf[S])
      } else if (m.isInstanceOf[AnyValManifest[_]]) {
        if (m == goalManifest) {
          Some(value.asInstanceOf[S])
        } else {
          None
        }
      } else if (m == goalManifest) {
        Some(value.asInstanceOf[S])
      } else if (goalManifest == manifest[Boolean] || goalManifest == manifest[java.lang.Boolean]) {
        if (value == null) {
          Some(false.asInstanceOf[S])
        } else {
          Some(true.asInstanceOf[S])
        }
      } else {
        None
      }
    }
  }
  
  class ElementaryMapExpression(expr: (String, ElementaryExpression)*) extends ElementaryExpression {
    override def extract[T](implicit m: Manifest[T]): Option[T] = 
      if (m == manifest[Map[String, ElementaryExpression]]) { 
        Some(map.asInstanceOf[T])
      } else if (m == manifest[String]) { 
        Some("{%s}".format(expr map { 
          case (k, v) => "\"%s\": %s".format(k, v.extractOrThrow[String]) } mkString ", " ).asInstanceOf[T])
      } else {
        None
      }
    
    def map: Map[String, ElementaryExpression] = expr toMap
    def lookup(str: String) = map(str)
  }

  class MapExpression(expr: (BaseExpression, BaseExpression)*) extends BaseExpression {
    override def isElementary = false
    override def eval(m: Machine) = 
      new ElementaryMapExpression(expr map { 
        case (k, v) => (k eval m).extractOrThrow[String] -> (v eval m) } :_*)
  }  
  
  class ElementaryListExpression(expr: ElementaryExpression*) extends ElementaryExpression {
    override def extract[T](implicit m: Manifest[T]): Option[T] = 
      if (m == manifest[List[ElementaryExpression]]) { 
        Some(toList.asInstanceOf[T])
      } else if (m == manifest[Boolean]) { 
        Some(toList.nonEmpty.asInstanceOf[T])
      } else if (m == manifest[String]) { 
        Some(("[%s]" format (expr map {  _.extractOrThrow[String] } mkString ", ")).asInstanceOf[T])
      } else {
        None
      }
    
    def toList: List[ElementaryExpression] = expr toList
  }

  class ListExpression(expr: BaseExpression*) extends BaseExpression{ 
    override def isElementary = false
    override def eval(m: Machine) = new ElementaryListExpression(expr map { _ eval m } :_*)
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
    override def extract[S](implicit manifest: Manifest[S]) = None
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
  object PlusFunction extends BinaryOperatorFunction("+", 4) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Int] + args(1).extractOrThrow[Int])
  }
  object ConcatFunction extends BinaryOperatorFunction("++", 4) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[String] + args(1).extractOrThrow[String])
  }
  object MinusFunction extends BinaryOperatorFunction("-", 4) { 
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
  object AndFunction extends BinaryOperatorFunction("and", 2) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Boolean] && args(1).extractOrThrow[Boolean])
  }
  object OrFunction extends BinaryOperatorFunction("or", 1)  {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Boolean] || args(1).extractOrThrow[Boolean])
  }
  object XorFunction extends BinaryOperatorFunction("xor", 1) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0).extractOrThrow[Boolean] ^ args(1).extractOrThrow[Boolean])
  }

  object EqualsFunction extends BinaryOperatorFunction("=", 3) {
    override protected def execute(args: ElementaryExpression*) = {
      new BasicExpression(args(0) == args(1))
    }
  }

  object LessThanFunction extends BinaryOperatorFunction("<", 3) {
    override protected def execute(args: ElementaryExpression*) = {
      new BasicExpression(args(0).extractOrThrow[Int] < args(1).extractOrThrow[Int])
    }
  }

  object LessThanOrEqualFunction extends BinaryOperatorFunction("<=", 3) {
    override protected def execute(args: ElementaryExpression*) = {
      new BasicExpression(args(0).extractOrThrow[Int] <= args(1).extractOrThrow[Int])
    }
  }

  object GreaterThanFunction extends BinaryOperatorFunction(">", 3) {
    override protected def execute(args: ElementaryExpression*) = {
      new BasicExpression(args(0).extractOrThrow[Int] > args(1).extractOrThrow[Int])
    }
  }

  object GreaterThanOrEqualFunction extends BinaryOperatorFunction(">=", 3) {
    override protected def execute(args: ElementaryExpression*) = {
      new BasicExpression(args(0).extractOrThrow[Int] >= args(1).extractOrThrow[Int])
    }
  }

  object NotEqualsFunction extends BinaryOperatorFunction("!=", 3) {
    override protected def execute(args: ElementaryExpression*) =
      new BasicExpression(args(0) != args(1))
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
                             XorFunction,      EqualsFunction, NotEqualsFunction,
                             ModulusFunction, ConcatFunction, 
                             LessThanFunction, LessThanOrEqualFunction, 
                             GreaterThanFunction, GreaterThanOrEqualFunction)
  val binaryOperatorMap = (binaryOperators map { op => op.name -> op })
    .toMap[String, BinaryOperatorFunction]
  
  val unaryOperators = List(NotFunction, UnaryMinusFunction)
  val unaryOperatorMap = (unaryOperators map { op => op.name -> op }) toMap

  abstract class ComplexExpression extends BaseExpression { 
    override def isElementary = false
  }
  
  class Pickout(from: BaseExpression, elem: BaseExpression) extends ComplexExpression { 
    override def eval(m: Machine) =  {
      val _from = from eval m
      val _elem = elem eval m

      ((_from.extract[List[ElementaryExpression]]) map { 
        lst => {
          val num = _elem.extractOrThrow[Int] 
          if (num >= lst.size) 
            throw except("Index out of bounds: %d in list %s", num, lst)
          lst(_elem.extractOrThrow[Int])
        }
      }) orElse (_from.extract[Map[String, ElementaryExpression]] map {
        map => {
          val str = _elem.extractOrThrow[String]
          map getOrThrow (str, except("No key: %s in map %s", str, map))
        }
      }) getOrThrow except("Expected list or map, got: %s", _from)
    }
  }

  class ApplicationExpression(val func: BaseExpression, val args: BaseExpression*) extends ComplexExpression {
    override def eval(m: Machine): ElementaryExpression = {
      func eval m match {
        case f: FunctionExpression => f(args map { _ eval m } :_*)
        case g => g 
      }
    }
  }
  
  class DottedExpression(val part1: BaseExpression, val part2: String) extends ComplexExpression { 
    override def eval(m: Machine): ElementaryExpression = {
      val obj = part1 eval m
      
      obj match { 
        case exp: UntypedExpression => {
          val inner = exp.extractOrThrow[AnyRef]
          
          val clz = inner getClass
          val method = clz getMethod part2

          method match {
            case null => throw except("Object %s lacks method %s.", obj.toString, part2)
            case m => new UntypedExpression(m invoke inner)
          }
        }
        case o => throw except("Expected UntypedExpression got a %s", o.toString)
      }
    }
  }

  class VariableExpression(variable: String) extends ComplexExpression { 
    override def eval(m: Machine): ElementaryExpression = 
      m.pad lookup variable getOrThrow except("%s is undefined" format variable)
  }
  
  class BaseElemExpression(out: HtmlOutput.BaseElem) extends ComplexExpression { 
    override def eval(m: Machine): ElementaryExpression = 
      new BasicExpression(HtmlEscapedString (out eval m))
  }

  def trueExpression = new BasicExpression[Boolean](true)
  def falseExpression = new BasicExpression[Boolean](false)
}
