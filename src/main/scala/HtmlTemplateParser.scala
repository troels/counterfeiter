package org.bifrost.counterfeiter

import scala.util.parsing.combinator.{ Parsers, RegexParsers, ImplicitConversions }
import scala.util.parsing.input.CharSequenceReader

import HtmlOutput._
import Expression.{BaseExpression, BasicExpression}

object HtmlTemplateParser extends RegexParsers with ImplicitConversions { 
  def ws = "[ \r\t\n]*".r
  def nonNl = "[^\r\n]*".r
  def forcedWs = "[ \r\t\n]+".r
  def wsNoNl = "[ \t]*".r
  def forcedWsNoNl = "[ \t]+".r
  def nl = "[\r\n]+".r

  def expression: Parser[BaseExpression] = '{' ~> ws ~> Parser[BaseExpression] {
    in => ExpressionParser.expression(in) match {
      case ExpressionParser.Success(res, next) => Success(res, next)
      case ExpressionParser.Error(msg, next) => Error(msg, next)
      case ns: ExpressionParser.NoSuccess => Error("Expression parse failure", in)
    }
  } <~ ws <~ '}'

  def verbatimExpression: Parser[BaseExpression] = 
    '"' ~> "[^\"]*".r <~ '"' ^^ (new BasicExpression[String](_))
    
  def tagAttributeList: Parser[Map[String, BaseExpression]] = 
    rep1sep(("[a-zA-Z]\\w*".r <~ ws <~ '=' <~ ws) ~ (expression | verbatimExpression) ^^ 
	    { case a ~ b => a -> b } , forcedWs) ^^ { 
      _ toMap 
    }
      
  def assembleTag(_tagName: Option[String], idAttr: Option[String], classAttrList: List[String], 
		  _attributeList: Option[Map[String, BaseExpression]]): Tag =  {
    val tagName = _tagName getOrElse "div" 
    val attributeList = _attributeList getOrElse Map()

    new Tag(tagName, attributeList ++ 
	    List(idAttr map { "id" -> new BasicExpression[String](_) },
		 if (classAttrList isEmpty) None else Some(
		   "class" -> new BasicExpression[String](classAttrList mkString " "))).flatten)
  }
		    

  def idPart = "#[a-zA-Z][-\\w_]*".r ^^ { _ substring 1 } 
  def classPart = "\\.[a-zA-Z][-\\w_]*".r ^^ { _ substring 1 }
  def tagPart: Parser[Tag] = 
    ((("[a-zA-Z]\\w*".r ^^ (Some(_))) ~ 
      opt(idPart) ~ rep(classPart)) |
     (success(Some("div")) ~ (idPart ^^ (Some(_))) ~ rep(classPart)) |
     (success(Some("div")) ~ success(None) ~ rep1(classPart))) ~ 
     opt('(' ~> ws ~> tagAttributeList <~ ws <~ ')') ^^ assembleTag
  
  
  def parseLinePrefix(indent: String): Parser[String] = 
    (indent ~ forcedWs) ^^ { case a ~ b => a + b }

  def htmlElem(indent: String): Parser[BaseElem] =
    tagElem(indent) | textElem(indent)

  def newLine[T](indent: String, cont: String => Parser[T]): Parser[T] = 
    Parser[T] { in =>
      guard(parseLinePrefix(indent))(in) match {
	case Success(res, next) => cont(res)(next)
	case ns: NoSuccess => ns 
      }
    }
  
  def parseElemsOnLevel(indent: String): Parser[List[BaseElem]] = 
    rep(indent ~> htmlElem(indent) <~ ws <~ nl) 
  
  def textElem(indent: String): Parser[BaseElem] = 
    ("| " ~> rep(
      ("[^\r\n{]*".r ^^ ( new Text(_) )) | ("{" ~> expression ^^ (new Expression(_))) <~ "}") ^^ {
	new ElemList(_ :_*) } ) <~ nl
		
  

  def tagElem(indent: String): Parser[BaseElem] = 
    ((rep1sep(tagPart, forcedWsNoNl) <~ wsNoNl <~ nl) ~ newLine(indent, parseElemsOnLevel)) ^^ {
      case _tagLst ~ content => {
	val tagLst = _tagLst reverse
	
	((tagLst tail) foldLeft ((tagLst head) addContent (content :_*))) { 
	  (innerTag, outerTag) => outerTag addContent innerTag 
	}
      }
    }
}
