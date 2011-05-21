package org.bifrost.counterfeiter

import scala.util.parsing.combinator.{ Parsers, RegexParsers, ImplicitConversions }
import scala.util.parsing.input.CharSequenceReader
import scala.collection.immutable.ListMap

import HtmlOutput._
import Expression.{BaseExpression, BasicExpression}

object HtmlTemplateParser extends RegexParsers with ImplicitConversions { 
  import U.Implicits._
  override protected val whiteSpace = "".r

  class HtmlTemplateParserException(msg: String) extends U.CounterFeiterException(msg: String)

  def ws = "[ \t\r\n]*".r
  def nonNl = "[^\r\n]*".r
  def forcedWs = "[ \r\t\n]+".r
  def wsNoNl = "[ \t]*".r
  def forcedWsNoNl = "[ \t]+".r
  def nl = "[ \r\t\n]*[\r\n]".r | (ws ~> elem(CharSequenceReader.EofCh))

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
    (rep1sep(tagPart, forcedWsNoNl) <~ wsNoNl <~ nl) ~ newIndent(indent, parseElemsOnLevel, EmptyElem) ^^ {
      case _tagLst ~ content => {
	val tagLst = _tagLst reverse
	
	((tagLst tail) foldLeft ((tagLst head) addContent content)) { 
	  (innerTag, outerTag) => outerTag addContent innerTag 
	}
      }
    }
  
  def namedArgs: Parser[(String, BaseExpression)] = 
    (wsNoNl ~> identifier <~ wsNoNl <~ '=') ~ (wsNoNl ~> expression ) ^^ tuplify

  def templateCallElem(indent: String): Parser[BaseElem] = 
    ('-' ~> wsNoNl ~> identifier ~ 
      rep(wsNoNl ~> '{' ~> wsNoNl ~> expression <~ wsNoNl <~ '}') ~ 
      (wsNoNl ~> rep(namedArgs)) <~ nl) ~
    newIndent(indent, parseArgsOnLevel, List()) ^^ { 
      case tmplName ~ posArgs ~ simpleNamedArgs ~ involvedNamedArgs => 
	new TemplateCall(tmplName, posArgs, 
			 simpleNamedArgs ++
			 (involvedNamedArgs map { case (id, v) => id -> new Expression.BaseElemExpression(v) }) toMap)
    }
  
    
  def parseLinePrefix(indent: String): Parser[String] = 
    (indent ~ forcedWs) ^^ { case a ~ b => a + b }

  def htmlElem(indent: String): Parser[BaseElem] =
    tagElem(indent) | textElem(indent) | ifElem(indent) | forElem(indent) | templateCallElem(indent)

  def newIndent[T](indent: String, cont: String => Parser[T], alt: T): Parser[T] = 
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
       newIndent(indent, parseElemsOnLevel, EmptyElem) ^^ { case expr ~ cons => {
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
    (expression <~ nl) ~
    newIndent(indent, parseElemsOnLevel, EmptyElem) ^^ {
	case variable ~ expr ~ clause => new ForElem(variable, expr, clause)
    }
  
  def identifier = "[a-zA-Z_][-\\w]*".r

  def tuplify[A,B](v: A ~ B) = (v._1 -> v._2)

  def parseArg(indent: String): Parser[(String, BaseElem)] = 
    (identifier <~ ':' <~ nl) ~ newIndent(indent, parseElemsOnLevel, EmptyElem) ^^ tuplify
      
  def parseArgsOnLevel(indent: String): Parser[List[(String, BaseElem)]] = rep(indent ~> parseArg(indent)) 

  def assembleTemplate(namespace: String)(name: String, firstArgs: List[(String, Option[BaseExpression])], 
		       secondArgs: List[(String, Option[BaseExpression])], body: BaseElem) = {
    val args = ListMap(firstArgs ++ secondArgs map { case (k, v) => (k -> (v map { _ eval EmptyMachine })) } :_*)
    new HtmlTemplate(name, namespace, args, body)
  }
  
  def templateDeclaration(namespace: String): Parser[HtmlTemplate] = 
    ("def" ~> forcedWsNoNl ~> identifier ~ rep(
      wsNoNl ~> identifier ~ opt(wsNoNl ~> '=' ~> wsNoNl ~> '{' ~> wsNoNl ~> expression <~ wsNoNl <~ '}') ^^ tuplify)
     <~ nl) ~ (newIndent("", parseArgsOnLevel, List()) ^^ { 
      lst => lst map { case (k, v) => k -> Some(new Expression.BaseElemExpression(v)) } } ) ~ 
       newIndent("", parseElemsOnLevel, EmptyElem) ^^ assembleTemplate(namespace)
  
  
  def namespaceDeclaration: Parser[String] =
    ("namespace" ~> forcedWsNoNl ~> rep1sep(identifier, '.') <~ nl) ^^ (_ mkString ".")
  
  def namespace: Parser[List[HtmlTemplate]] =
    Parser[List[HtmlTemplate]] { in =>
      ("(?:[ \r\t\n]*[\r\n])?".r ~> namespaceDeclaration)(in) match {
	case Success(namespace, next) => rep1("(?:[ \r\t\n]*[\r\n])?".r ~> templateDeclaration(namespace))(next)
	case o: NoSuccess => rep1("(?:[ \r\t\n]*[\r\n])?".r ~> templateDeclaration(""))(in)
      }
     }
  
  def parseModule(str: String): Machine = 
    (rep1(namespace))(new CharSequenceReader(str)) match {
      case Success(res, next) if next.atEnd => new Machine(
	res.flatten map { ht => (ht fullName) -> ht } toMap, BasicFunctions.standardPad)
      case Success(res, next) =>  
	throw new HtmlTemplateParserException("Garbage at end of file:\n%s".format(next.pos.longString))
      case ns: NoSuccess => 
	throw new HtmlTemplateParserException("Failed to parser because of: %s".format(ns.msg))
    }
}
