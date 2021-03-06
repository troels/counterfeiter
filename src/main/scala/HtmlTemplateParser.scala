package org.bifrost.counterfeiter

import scala.util.parsing.combinator.{ Parsers, RegexParsers, ImplicitConversions }
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.CharSequenceReader.EofCh
import scala.collection.immutable.ListMap

import HtmlOutput._
import Expression.{BaseExpression, BasicExpression, ComplexExpression}

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
    '"' ~> rep("[^\"\\\\]+".r | ("\\\\[\"\\\\]".r ^^ (_ substring 1))) <~ '"' ^^ { 
      expr => new BasicExpression[String](expr mkString "")
    }
    
  def tagAttributeList: Parser[Map[String, BaseExpression]] = 
    ws ~> rep1sep(("[a-zA-Z][-_\\w]*".r <~ ws <~ '=' <~ ws) ~ (('{' ~> ws ~> expression <~ ws <~ '}') | verbatimExpression) ^^ 
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
     opt(ws ~> '(' ~> ws ~> tagAttributeList <~ ws <~ ')') ^^ assembleTag

  def continueAndFindNewIndent(indent: String): Parser[BaseElem] = {
    ((wsNoNl ~> htmlElem(indent) ^^ (Some(_))) | (wsNoNl ~> nl ^^^ None)) ~  
      newIndent(indent, parseElemsOnLevel, Some(EmptyElem)) ^^ {
       case opt ~ rest => opt map { x => new ElemList(x, rest) } getOrElse rest
     }
  }

  def cssIdentifier = "[\\w_-]+".r
  def cssLines(indent: String): Parser[List[(String, BaseElem)]] = 
    rep1(indent ~>
         rep1(
           (wsNoNl ~> cssIdentifier <~ ':' <~ wsNoNl) ~ ((
             ("[^;\r\n{]+".r ^^ (new Text(_))) |
             ("{" ~> wsNoNl ~> expression ^^ (new Expression(_))) <~ wsNoNl <~ '}' ) <~ wsNoNl <~ ';'
            )) 
         <~ nl) ^^ {
      lst => lst flatMap { _ map (tuplify _) }
    }
  
    
  def tagElem(indent: String): Parser[BaseElem] = 
    (tagPart ~ (((nl ~> newIndent(indent, cssLines)) ~ newIndent(indent, parseElemsOnLevel, Some(EmptyElem)))
                | (success(List()) ~ continueAndFindNewIndent(indent)))) ^^ {
     case tag ~ (css ~ rest) => {
       val tagWithStyle = if (css isEmpty)
         tag 
       else
         tag addAttrib (
           "style", new ComplexExpression {
             override def eval(m: Machine) = 
               new BasicExpression[String](css map { case (k, v) => "%s: %s" format (k, v eval m) } mkString "; ")})
       tagWithStyle addContent rest
      }
    }
  
  def namedArgs: Parser[(String, BaseExpression)] = 
    (wsNoNl ~> identifier <~ wsNoNl <~ '=') ~ (wsNoNl ~> expression ) ^^ tuplify
  
  def templateCallElem(indent: String): Parser[BaseElem] = 
    ('-' ~> wsNoNl ~> fullyQualifiedIdentifier ~ 
      rep(wsNoNl ~> '{' ~> wsNoNl ~> expression <~ wsNoNl <~ '}') ~ 
      (wsNoNl ~> rep(namedArgs)) <~ nl) ~
    newIndent(indent, parseArgsOnLevel, Some(List())) ^^ { 
      case tmplName ~ posArgs ~ simpleNamedArgs ~ involvedNamedArgs => 
	      new TemplateCall(tmplName, posArgs, simpleNamedArgs ++
			                   (involvedNamedArgs map { case (id, v) => id -> new Expression.BaseElemExpression(v) }) toMap)
    }
  
    
  def parseLinePrefix(indent: String): Parser[String] = 
    (indent ~ forcedWs) ^^ { case a ~ b => a + b }

  def htmlElem(indent: String): Parser[BaseElem] =
    tagElem(indent) | textElem(indent) | ifElem(indent) | forElem(indent) | letElem(indent) | templateCallElem(indent)

  def newIndent[T](indent: String, cont: String => Parser[T], alt: Option[T] = None): Parser[T] = 
    Parser[T] { in =>
      guard(parseLinePrefix(indent))(in) match {
	      case Success(res, next) => cont(res)(next) 
	      case ns: NoSuccess => alt map { Success(_, in) } getOrElse ns
      }
    }
  
  def parseElemsOnLevel(indent: String): Parser[BaseElem] = 
    rep(indent ~> htmlElem(indent)) ^^ ( lst => new ElemList(lst intersperse (new Text("\n")) :_*) )

  def ifElem(indent: String): Parser[BaseElem] = {
    def ifClause(ifPhrase: String, ind: String, expr: Option[BaseExpression] = None) = {
      (ind ~ "+" ~> wsNoNl ~> ifPhrase ~> wsNoNl ~> (expr map (success(_)) getOrElse expression) <~ wsNoNl <~ nl) ~ 
       newIndent(indent, parseElemsOnLevel, Some(EmptyElem)) ^^ { case expr ~ cons => {
	       (expr, cons) 
       } } }
    (ifClause("if", "") ~ rep(ifClause("elsif", indent)) ~ 
     opt(ifClause("else", indent, Some(Expression.trueExpression)))) ^^ {
       case main ~ elsifs ~ els => new IfElem((main +: elsifs) ++ (els toList) :_*)
    }
  }

  def textElem(indent: String): Parser[BaseElem] = 
    "|" ~> wsNoNl ~> 
      (rep(
	      ("[^\r\n{]+".r ^^ (new Text(_))) | 
	      ("{" ~> wsNoNl ~> expression ^^ (new Expression(_))) <~ wsNoNl <~ '}') ^^ {
	        new ElemList(_ :_*) 
	      }) <~ wsNoNl <~ nl
    
  
  def forElem(indent: String): Parser[BaseElem] = 
    ("+" ~> wsNoNl ~> "for" ~> wsNoNl ~> "[a-zA-Z_][\\w_]*".r <~ wsNoNl <~ "in" <~ wsNoNl) ~
    (expression <~ nl) ~
    newIndent(indent, parseElemsOnLevel, Some(EmptyElem)) ^^ {
	    case variable ~ expr ~ clause => new ForElem(variable, expr, clause)
    }
  
  def letElem(indent: String): Parser[BaseElem] = 
    ("+" ~> wsNoNl ~> "let" ~> wsNoNl ~> "[a-zA-Z_][\\w_]*".r <~ wsNoNl <~ "=" <~ wsNoNl) ~ (expression <~ nl) ~
    newIndent(indent, parseElemsOnLevel, Some(EmptyElem)) ^^ {
	    case variable ~ expr ~ clauses => new LetElem(variable, expr, clauses)
    }

  def identifier = "[a-zA-Z_][-\\w]*".r
  
  def fullyQualifiedIdentifier = 
    "[a-zA-Z_][-\\w]*(?:\\.[a-zA-Z_][-\\w]*)*".r

  def tuplify[A,B](v: A ~ B) = (v._1 -> v._2)

  def parseArg(indent: String): Parser[(String, BaseElem)] = 
    (identifier <~ ':') ~ (continueAndFindNewIndent(indent)) ^^ tuplify
      
  def parseArgsOnLevel(indent: String): Parser[List[(String, BaseElem)]] = rep(indent ~> parseArg(indent))

  def assembleTemplate(namespace: String)(name: String, firstArgs: List[(String, Option[BaseExpression])], 
		       secondArgs: List[(String, Option[BaseExpression])], body: BaseElem) = {
    val args = ListMap(firstArgs ++ secondArgs map { case (k, v) => (k -> (v map { _ eval EmptyMachine })) } :_*)
    new HtmlTemplate(name, namespace, args, body)
  }
  
  def templateDeclaration(namespace: String): Parser[HtmlTemplate] = 
    ("def" ~> forcedWsNoNl ~> identifier ~ rep(
      wsNoNl ~> identifier ~ opt(wsNoNl ~> '=' ~> wsNoNl ~> '{' ~> wsNoNl ~> expression <~ wsNoNl <~ '}') ^^ tuplify)
     <~ nl) ~ (newIndent("", parseArgsOnLevel, Some(List())) ^^ { 
      lst => lst map { case (k, v) => k -> Some(new Expression.BaseElemExpression(v)) } } ) ~ 
       newIndent("", parseElemsOnLevel, Some(EmptyElem)) ^^ assembleTemplate(namespace)
  
  
  def namespaceDeclaration: Parser[String] =
    "namespace" ~> forcedWsNoNl ~> fullyQualifiedIdentifier <~ nl
  
  def namespace: Parser[List[HtmlTemplate]] =
    Parser[List[HtmlTemplate]] { in =>
      ("(?:[ \r\t\n]*[\r\n])?".r ~> namespaceDeclaration)(in) match {
	      case Success(namespace, next) => rep1("(?:[ \r\t\n]*[\r\n])?".r ~> templateDeclaration(namespace))(next)
	      case o: NoSuccess => rep1("(?:[ \r\t\n]*[\r\n])?".r ~> templateDeclaration(""))(in)
      }
     }
  
  def parseModule(str: String): TemplateCollection = 
    (rep1(namespace))(new CharSequenceReader(str)) match {
      case Success(res, next) => {
	      ("\\s*".r ~> EofCh)(next) match { 
	        case Success(_, next) => new TemplateCollection(res flatten)
	        case ns: NoSuccess => 
	          throw new HtmlTemplateParserException("Garbage at end of file:\n%s".format(next.pos.longString))
	      }
      }
      case ns: NoSuccess => 
	      throw new HtmlTemplateParserException("Failed to parser because of: %s".format(ns.msg))
    }
}
