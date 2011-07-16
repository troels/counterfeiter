package org.bifrost.counterfeiter

import scala.util.parsing.combinator.{ Parsers, RegexParsers, ImplicitConversions }
import scala.util.parsing.input.CharSequenceReader
import Expression._

object ExpressionParser extends RegexParsers { 

  override protected val whiteSpace = "".r

  class CounterfeiterParserException(msg: String) extends U.CounterFeiterException(msg)
  
  def except(format: String, args: Any*) = 
    () => new CounterfeiterParserException(format.format(args :_*))

  def ws = "[ \t]*".r
  def string: Parser[String] = ('"' ~> "(?:[^\"\\\\]+|\\\\.)+".r <~ '"') ^^ { 
    str => str.replaceAll("\\\\(.)", "$1")
  }

  def number: Parser[Int] = "\\d+".r ^^ (_ toInt)
  
  def stringExpression = string ^^ (new BasicExpression[String](_))
  def numberExpression = number ^^ (new BasicExpression[Int](_))
  def booleanExpression: Parser[ElementaryExpression] = 
    ("true(?!\\w)".r ^^^ (new BasicExpression[Boolean](true))) | 
    ("false(?!\\w)".r ^^^ (new BasicExpression[Boolean](false)))
  
  def listExpression: Parser[ListExpression] = 
    ws ~> "[" ~> repsep(ws ~> expression <~ ws, ",") <~ "]" <~ ws ^^ { lst =>
      new ListExpression(lst :_*) }
  
  def mapExpression: Parser[MapExpression] = 
    ws ~> "{" ~> repsep((ws ~> expression <~ ws <~ ':' <~ ws) ~ (expression <~ ws), ',') <~ "}" <~ ws ^^ {
      exprs => new MapExpression(exprs map { case k ~ v => k -> v } :_* ) }

  def operator: Parser[FunctionExpression] = binaryOperator | unaryOperator

  def binaryOperator: Parser[BinaryOperatorFunction] = 
    "[-+*/=]|!=|(?:(?:and|or|xor)(?!\\w))".r ^^ (Expression.binaryOperatorMap(_))
  
  def unaryOperator: Parser[UnaryOperatorFunction] = 
    "-|(?:not(?!\\w))".r ^^ (Expression.unaryOperatorMap(_))

  def functionCall: Parser[ApplicationExpression] = 
    (identifier <~ ws) ~ ('(' ~> repsep(ws ~> expression <~ ws, ',') <~ ')') ^^ {
      case (id ~ args) => new ApplicationExpression(id, args: _*)
    }

  def identifierRegex = "[a-zA-Z_][\\w_]*".r
  def identifier: Parser[BaseExpression] = not(operator) ~> (
    (identifierRegex ^^ (new VariableExpression(_))) | 
    ('(' ~> ws ~> operator <~ ws <~ ')'))
  
  def subexpression: Parser[BaseExpression] = '(' ~> ws ~> expression <~ ws <~ ')'
  
  abstract class LinePart
  case class Term(expr: BaseExpression) extends LinePart
  case class BinaryOperator(op: BinaryOperatorFunction) extends LinePart
  
  class SEHelper()
  case class StringKeeper(str: String) extends SEHelper
  case class SEKeeper(se: BaseExpression) extends SEHelper
  def simpleExpression: Parser[BaseExpression] = 
    (ws ~> (
      stringExpression | numberExpression | booleanExpression | 
      listExpression | mapExpression | identifier | subexpression) ~ 
     rep(('.' ~> identifierRegex ^^ (StringKeeper(_))) | 
         (ws ~> '[' ~> ws ~> (simpleExpression ^^ (SEKeeper(_))) <~ ws <~ ']'))) ^^ { 
	   case expr ~ lookups => 
	     (lookups foldLeft expr) {
	       (accum, lookup) => 
           lookup match { 
             case StringKeeper(str) => new DottedExpression(accum, str)
             case SEKeeper(se) => new Pickout(accum, se)
	         }
       }
  }
	    

  def simpleExpressionWithUnaryPrefix: Parser[BaseExpression] = 
    ws ~> rep(ws ~> unaryOperator) ~ simpleExpression  ^^ { 
      case lst ~ se => (lst foldRight se) { (op, operand) => new ApplicationExpression(op, operand) } 
    }

  def term: Parser[Term] =
    ws ~> simpleExpressionWithUnaryPrefix ~ rep(ws ~> simpleExpression) ^^ {
      case expr0 ~ rest => Term(new ApplicationExpression(expr0, rest :_*))
    }
    
  def binaryExpression: Parser[List[LinePart]] = 
    term ~  rep(ws ~> binaryOperator ~ (ws ~> term)) ^^ { 
      case lp0 ~ lst =>
	      lp0 :: ( lst flatMap { case op ~ lp => List(BinaryOperator(op) , lp)} )
    }

  def expression: Parser[BaseExpression] = 
    (ws ~> binaryExpression) ^^ { x => parseLineParts(x) match {
      case Term(t) :: Nil => t
      case o => throw except("Expected list with one term, got: %s", o)()
    }
  }

  def parseLineParts(lst: List[LinePart], priority: Int = 0): List[LinePart] = {
    lst match {
      case Nil => throw except("List is empty")()
      case head :: Nil => lst

      // Parser operator
      case Term(t) :: (op: BinaryOperator) ::  tail => {
	      if (op.op.priority <= priority) {
	        lst
	      } else if (tail isEmpty) {
	        List(Term(new ApplicationExpression(op.op, t)))
	        } else {
	          parseLineParts(tail, op.op.priority) match {
	            case Term(t2) :: tail => parseLineParts(
	              Term(new ApplicationExpression(op.op, t, t2)) :: tail, priority)
	            case o => throw except("Internal error, excepted list with term as start, got: %s", o)()
	          }
	        }
      }
      
      case o => {
	      throw except("Failed to do anythin sensible with %s", o.toString)()
      }
    }
  }
}
  
