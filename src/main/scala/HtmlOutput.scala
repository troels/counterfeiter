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
			    
  implicit def string2text(str: String): Text = Text(str)
  implicit def text2string(text: Text): String = text.text
  
  case class Text(text: String) extends BaseElem {
    override def eval(pad: Pad): String = text
  }
  
  object AttributeList { 
    def apply(attribs: (String, BaseExpression)*) = 
      new AttributeList(attribs :_*)
    
    def empty = AttributeList()
  }

  class AttributeList(attribs: (String, BaseExpression)*) {
    assert (attribs forall { case (k, _) => U.isWord(k) } )
      
    val attributes = ListMap[String, BaseExpression](
      attribs map { case (k, v) => k.toLowerCase -> v } :_*)

    def eval(pad: Pad): String = 
      attributes map { 
	case (k, v) => "%s=\"%s\"".format(k, v eval pad toString)
      } mkString " "
  }

  class Tag(tag: String="div", attributes: AttributeList=AttributeList(), content: List[BaseElem]=List()) 
  extends BaseElem {
    override def eval(pad: Pad): String = 
      if (content isEmpty) 
	"<%s %s/>".format(tag, attributes eval pad)
      else 
	"<%s %s>%s</%s>".format(tag, attributes eval pad, content map ( _ eval pad ) mkString "\n", tag)
  }

  class Expression(expression: BaseExpression) extends BaseElem { 
    override def eval(pad: Pad): String = (expression eval pad).extractOrThrow[String]
  }
  
  class ElemList(elems: BaseElem*) extends BaseElem {
    override def eval(pad: Pad) = elems map ( _ eval pad) mkString "\n"
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

