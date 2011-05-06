package org.bifrost.counterfeiter

import org.bifrost.counterfeiter.Expression.BaseExpression
import scala.collection.immutable.ListMap

object  HtmlOutput {
  abstract sealed class BaseElem { 
    def eval(pad: Pad): String
  }
  
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
    override def eval(pad: Pad): String = (expression eval pad).extract[String]
  }

  class HtmlTemplate(
}

