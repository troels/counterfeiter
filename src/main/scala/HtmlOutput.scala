package org.bifrost.counterfeiter

import org.bifrost.counterfeiter.Expression.{BaseExpression, ElementaryExpression}
import scala.collection.immutable.ListMap

object HtmlOutput {
  abstract sealed class BaseElem { 
    def eval(pad: Pad): String
  }
  
  class HtmlOutputException(msg: String) extends U.CounterFeiterException(msg: String)
  
  def except(format: String, args: Any*) =
    new HtmlOutputException(format.format(args :_*))
  
  class Text(text: String) extends BaseElem {
    override def eval(pad: Pad): String = text
  }

  class Tag(tag: String, attributes: Map[String, BaseExpression], content: List[BaseElem]=List()) 
  extends BaseElem {
    def outputAttribs(pad: Pad): String = 
      attributes map { 
	case (k, v) => "%s=\"%s\"".format(k, (v eval pad).extractOrThrow[String])
      } mkString " "

    override def eval(pad: Pad): String = 
      if (content isEmpty) 
	"<%s %s/>".format(tag, outputAttribs(pad))
      else 
	"<%s %s>%s</%s>".format(tag, outputAttribs(pad), content map ( _ eval pad ) mkString "\n", tag)

    def addContent(content: BaseElem*): Tag = 
      new Tag(tag, attributes, this.content ++ content)
  }

  class Expression(expression: BaseExpression) extends BaseElem { 
    override def eval(pad: Pad): String = (expression eval pad).extractOrThrow[String]
  }
  
  class ElemList(elems: BaseElem*) extends BaseElem {
    override def eval(pad: Pad) = elems map ( _ eval pad) mkString "\n"
  }
  
  class IfElem(clauses: (BaseExpression, BaseElem)*) extends BaseElem{
    override def eval(pad: Pad) = 
      clauses find { case (cond, _) => (cond eval pad).extractOrThrow[Boolean] } map {
	case (_, elem) => elem eval pad } getOrElse ""
  }

  class HtmlTemplate(name: String, argumentTemplate: ListMap[String, Option[ElementaryExpression]], 
		     content: BaseElem) {
    def renderTemplate(namedArguments: Map[String, ElementaryExpression]): String = {
      val allArguments: Map[String, ElementaryExpression] = argumentTemplate map {
	case (name, default) => 
	  namedArguments get name orElse default match {
	    case Some(value) => (name -> value)
	    case None => throw except("Missing argument: %s", name)
	  }
      }

      val pad = new VariablePad(allArguments)
      content eval pad
    }

    def renderTemplate(
      positionalArguments: List[ElementaryExpression], namedArguments: Map[String, ElementaryExpression]): String = {
      val newNamedArgs = positionalArguments zip argumentTemplate map {
	case (arg, (name, _)) => 
	  if (namedArguments contains name) 
	    throw except("Argument %s given twice".format(name))
	  else 
	    (name -> arg)
      }

	renderTemplate(namedArguments ++ newNamedArgs)
    }
    
    def renderTemplate(positionalArguments: List[ElementaryExpression]): String =
      renderTemplate(positionalArguments, Map())
  }
}

