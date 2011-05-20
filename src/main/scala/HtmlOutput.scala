package org.bifrost.counterfeiter

import org.bifrost.counterfeiter.Expression.{BaseExpression, ElementaryExpression}
import scala.collection.immutable.ListMap

object HtmlOutput {
  abstract sealed class BaseElem { 
    def eval(m: Machine): String
  }
  
  class HtmlOutputException(msg: String) extends U.CounterFeiterException(msg: String)
  
  def except(format: String, args: Any*) =
    new HtmlOutputException(format.format(args :_*))
  
  class Text(text: String) extends BaseElem {
    override def eval(m: Machine): String = text
  }

  class Tag(tag: String, attributes: Map[String, BaseExpression], content: List[BaseElem]=List()) 
  extends BaseElem {
    def outputAttribs(m: Machine): String = 
      attributes map { 
	case (k, v) => "%s=\"%s\"".format(k, (v eval m).extractOrThrow[String])
      } mkString " "

    override def eval(m: Machine): String = {
      val attribs = outputAttribs(m)
      val attribsOut = if (attribs isEmpty) "" else " " + attribs
      val evaledContent = content map ( _ eval m ) mkString "\n"
      if (evaledContent isEmpty) 
	"<%s%s/>".format(tag, attribsOut)
      else 
	"<%s%s>%s</%s>".format(tag, attribsOut, evaledContent, tag)
    }

    def addContent(content: BaseElem*): Tag = 
      new Tag(tag, attributes, this.content ++ content)
    
    override def toString = 
      "Tag: %s, %s: [\n%s\n]".format(tag, attributes, content map { _.toString } mkString "\n")
  }

  object EmptyElem extends BaseElem { 
    override def eval(m: Machine): String = ""
  }

  class Expression(expression: BaseExpression) extends BaseElem { 
    override def eval(m: Machine): String = (expression eval m).extractOrThrow[String]
  }
  
  class ElemList(elems: BaseElem*) extends BaseElem {
    override def eval(m: Machine) = 
      elems map ( _ eval m) mkString ""
    
    override def toString = "ElemList:[\n%s\n]".format(elems map { _.toString } mkString "\n")
  }

  class IfElem(clauses: (BaseExpression, BaseElem)*) extends BaseElem{
    override def eval(m: Machine) = 
      clauses find { case (cond, _) => (cond eval m).extractOrThrow[Boolean] } map {
	case (_, elem) => elem eval m } getOrElse ""
  }

  class ForElem(variable: String, range: BaseExpression, clauses: BaseElem) extends BaseElem{
    override def eval(m: Machine) = {
      (range eval m).extractOrThrow[List[ElementaryExpression]] map { 
	i: ElementaryExpression => clauses eval (m newPad Map(variable -> i))
      } mkString ""
    }
  }

  class CallTemplate(templateName: String, positionalArguments: List[BaseExpression], 
		     namedArguments: Map[String, BaseExpression]) extends BaseElem {
    override def eval(m: Machine) = {
      m.renderTemplate(templateName, positionalArguments map { _ eval m },
		       namedArguments mapValues  { _ eval m })
    }
  }

  class HtmlTemplate(val name: String, argumentTemplate: ListMap[String, Option[ElementaryExpression]], 
		     content: BaseElem) {
    def renderTemplate(m: Machine, namedArguments: Map[String, ElementaryExpression]): String = {
      val allArguments = argumentTemplate map {
	case (name, default) => 
	  namedArguments get name orElse default match {
	    case Some(value) => (name -> value)
	    case None => throw except("Missing argument: %s", name)
	  }
      }

      content eval (m newPad allArguments)
    }

    def renderTemplate(m: Machine,
      positionalArguments: List[ElementaryExpression], namedArguments: Map[String, ElementaryExpression]): String = {
      val newNamedArgs = positionalArguments zip argumentTemplate map {
	case (arg, (name, _)) => 
	  if (namedArguments contains name) 
	    throw except("Argument %s given twice".format(name))
	  else 
	    (name -> arg)
      }

	renderTemplate(m, namedArguments ++ newNamedArgs)
    }
    
    def renderTemplate(m: Machine, positionalArguments: List[ElementaryExpression]): String =
      renderTemplate(m, positionalArguments, Map())
  }
}
