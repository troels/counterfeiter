package org.bifrost.counterfeiter

import scala.math
import java.net.URLEncoder
import java.util.UUID

object BasicFunctions {
  import Expression.{FunctionExpression, ElementaryExpression, BasicExpression}
  
  case class StandardFunctionExpression(
    name: String, numberOfArgs: Int, f: (ElementaryExpression*) => ElementaryExpression) 
  extends FunctionExpression {
    override protected def execute(args: ElementaryExpression*) = f(args :_*)
  }

  def argumentDeconstructor[R, S](f: R => S)(implicit mr: Manifest[R], ms: Manifest[S]) = 
    {
      def g(args: ElementaryExpression*) = f(args(0).extractOrThrow[R])
      g _
    }

  def argumentDeconstructor[R, S, T](f: (R, S) => T)(implicit mr: Manifest[R], ms: Manifest[S], mt: Manifest[T]) = 
    {
      def g(args: ElementaryExpression*) = f(args(0).extractOrThrow[R], args(1).extractOrThrow[S])
      g _
    }

  def argumentDeconstructor[R, S, T, U](f: (R, S, T) => U)
                                       (implicit mr: Manifest[R], ms: Manifest[S], mt: Manifest[T], mu: Manifest[U]):
    (ElementaryExpression*) => U = {
    def g(args:  ElementaryExpression*) = 
      f(args(0).extractOrThrow[R], args(1).extractOrThrow[S], args(2).extractOrThrow[T])
    g _
  }

  
  def substring = StandardFunctionExpression(
    "substring", 3, argumentDeconstructor[String, Int, Int, ElementaryExpression] { 
      (str, _start, len) => {
	      val strLen = str.length
	      val start = math.min(math.max(_start, 0), strLen)
	      val end = math.min(start + len, strLen)
	      new BasicExpression[String](str substring (start, end))
      }
    } 
  )
  
  def escaped = new FunctionExpression { 
    override def name = "e"
    override def numberOfArgs = 1
    
    override def execute(args: ElementaryExpression*): ElementaryExpression =
      new BasicExpression[HtmlEscapedString](
        args(0).extract[HtmlEscapedString] getOrElse 
        HtmlEscapedString(args(0).extractOrThrow[String]))
  }
  
  def urlencode = new FunctionExpression { 
    override def name = "u" 
    override def numberOfArgs = 1
    
    override def execute(args: ElementaryExpression*): ElementaryExpression = 
      new BasicExpression[String](
        URLEncoder encode (args(0).extractOrThrow[String], "UTF-8"))
  }
  
  def if_ = new FunctionExpression { 
    override def name = "if" 
    override def numberOfArgs = 3
    
    override def execute(args: ElementaryExpression*): ElementaryExpression = 
      if (args(0).extractOrThrow[Boolean]) { 
        args(1) 
      } else {
        args(2)
      }
  }

  def guid = new FunctionExpression { 
    override def name = "guid"
    override def numberOfArgs = 0
    
    override def execute(args: ElementaryExpression*): ElementaryExpression = 
      new BasicExpression[String](UUID.randomUUID toString)
  }

  val functionList = List(substring, escaped, urlencode, guid, if_)
  
  val standardPad = new VariablePad(functionList map { f => f.name -> f } toMap)
}
