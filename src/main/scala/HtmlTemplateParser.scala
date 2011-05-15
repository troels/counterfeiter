package org.bifrost.counterfeiter

import scala.util.parsing.combinator.{ Parsers, RegexParsers, ImplicitConversions }
import scala.util.parsing.input.CharSequenceReader

import HtmlOutput._
import Expression.{BaseExpression, BasicExpression}

object HtmlTemplateParser extends RegexParsers with ImplicitConversions { 
  import U.Implicits._
  override protected val whiteSpace = "".r
  
  def ws = "[ \t\r\n]*".r
  def nonNl = "[^\r\n]*".r
  def forcedWs = "[ \r\t\n]+".r
  def wsNoNl = "[ \t]*".r
  def forcedWsNoNl = "[ \t]+".r
  def nl = "[\r\n]+".r

  def expression: Parser[BaseExpression] = Parser[BaseExpression] {
    in => ExpressionParser.expression(in) match {
      case ExpressionParser.Success(res, next) => Success(res, next)
      case ExpressionParser.Error(msg, next) => Error(msg, next)
      case ns: ExpressionParser.NoSuccess => Error("Expression parse failure", in)
    }
  }

  def verbatimExpression: Parser[BaseExpression] = 
    '"' ~> "[^\"]*".r <~ '"' ^^ (new BasicExpression[String](_))
    
  def tagAttributeList: Parser[Map[String, BaseExpression]] = 
    rep1sep(("[a-zA-Z]\\w*".r <~ ws <~ '=' <~ ws) ~ (('{' ~> ws ~> expression <~ ws <~ '}') | verbatimExpression) ^^ 
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

  def tagElem(indent: String): Parser[BaseElem] = 
    (rep1sep(tagPart, forcedWsNoNl) <~ wsNoNl <~ nl) ~ newLine(indent, parseElemsOnLevel, EmptyElem) ^^ {
      case _tagLst ~ content => {
	val tagLst = _tagLst reverse
	
	((tagLst tail) foldLeft ((tagLst head) addContent content)) { 
	  (innerTag, outerTag) => outerTag addContent innerTag 
	}
      }
    }
  
  def parseLinePrefix(indent: String): Parser[String] = 
    (indent ~ forcedWs) ^^ { case a ~ b => a + b }

  def htmlElem(indent: String): Parser[BaseElem] =
    tagElem(indent) | textElem(indent) | ifElem(indent) | forElem(indent)

  def newLine[T](indent: String, cont: String => Parser[T], alt: T): Parser[T] = 
    Parser[T] { in =>
      guard(parseLinePrefix(indent))(in) match {
	case Success(res, next) => cont(res)(next)
	case ns: NoSuccess => Success(alt, in)
      }
    }
  
  def parseElemsOnLevel(indent: String): Parser[BaseElem] = 
    rep(indent ~> htmlElem(indent)) ^^ ( lst => new ElemList(lst intersperse (new Text("\n")) :_*) )

  def ifElem(indent: String): Parser[BaseElem] = {
    def ifClause(ifPhrase: String, ind: String, expr: Option[BaseExpression] = None) = {
      (ind ~ "+" ~> wsNoNl ~> ifPhrase ~> wsNoNl ~> (expr map (success(_)) getOrElse expression) <~ wsNoNl <~ nl) ~ 
       newLine(indent, parseElemsOnLevel, EmptyElem) ^^ { case expr ~ cons => {
	 (expr, cons) 
       } } }
    (ifClause("if", "") ~ rep(ifClause("elsif", indent)) ~ 
     opt(ifClause("else", indent, Some(Expression.trueExpression)))) ^^ {
      case main ~ elsifs ~ els => new IfElem((main +: elsifs) ++ (els toList) :_*)
    }
  }

  def textElem(indent: String): Parser[BaseElem] = 
    ("|" ~> ws ~> rep(
      ("[^\r\n{]+".r ^^ (new Text(_))) | ("{" ~> wsNoNl ~> expression ^^ (new Expression(_))) <~ wsNoNl <~ '}') ^^ {
	new ElemList(_ :_*) } ) <~ nl
    
  
  def forElem(indent: String): Parser[BaseElem] = 
    ("+" ~> wsNoNl ~> "for" ~> wsNoNl ~> "[a-zA-Z_][\\w_]*".r <~ wsNoNl <~ "in" <~ wsNoNl) ~
    (expression <~ wsNoNl <~ nl) ~
    newLine(indent, parseElemsOnLevel, EmptyElem) ^^ {
	case variable ~ expr ~ clause => new ForElem(variable, expr, clause)
      }
}
