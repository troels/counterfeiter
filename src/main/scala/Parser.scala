package org.bifrost.counterfeiter

import scala.util.parsing.combinator.{ Parsers, RegexParsers, ImplicitConversions }
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers { 
  import Expression._

  class CounterfeiterParserException(msg: String) extends U.CounterFeiterException(msg)
  
  def except(format: String, args: Any*) = 
    () => new CounterfeiterParserException(format.format(args :_*))

  def ws = "[ \r\t\n]*".r
  def string: Parser[String] = '"' ~> "(?:[^\"\\]+|\\.)+".r <~ '"'
  def number: Parser[Int] = "\\d+".r ^^ (_ toInt)
  
  def stringExpression = string ^^ (new BasicExpression[String](_))
  def numberExpression = number ^^ (new BasicExpression[Int](_))
  def booleanExpression: Parser[ElementaryExpression] = 
    ("true(?!\\w)".r ^^^ (new BasicExpression[Boolean](true))) | 
    ("false(?!\\w)".r ^^^ (new BasicExpression[Boolean](false)))
  
  def operator: Parser[FunctionExpression] = binaryOperator | unaryOperator
  def binaryOperator: Parser[BinaryOperatorFunction] = 
    "[-+*/=]|!=|(?:(?:and|or|xor)(?!\\w))".r ^^ (Expression.binaryOperatorMap(_))
  
  def unaryOperator: Parser[UnaryOperatorFunction] = 
    "-|(?:not(?!\\w))".r ^^ (Expression.unaryOperatorMap(_))

  def functionCall: Parser[ApplicationExpression] = 
    (identifier <~ ws) ~ ('(' ~> repsep(ws ~> expression <~ ws, ',') <~ ')') ^^ {
      case (id ~ args) => new ApplicationExpression(id, args: _*)
    }

  def identifier: Parser[BaseExpression] = not(operator) ~> (
    ("[a-zA-Z_][\\w_]*(\\.[a-zA-Z_][\\w_]*)*".r ^^ (new VariableExpression(_))) | 
    ('(' ~> ws ~> operator <~ ws <~ ')'))
  
  def subexpression: Parser[BaseExpression] = '(' ~> ws ~> expression <~ ws <~ ')'
  
  abstract class LinePart
  case class Term(term: BaseExpression, unaryOps: List[UnaryOperatorFunction] = List()) extends LinePart {
    def base: BaseExpression = 
      (unaryOps foldRight term) { (op, operand) => new ApplicationExpression(op, operand) }
  }
  case class BinaryOperator(op: BinaryOperatorFunction) extends LinePart

  def linePart: Parser[LinePart] =
    (ws ~> rep(ws ~> unaryOperator <~ ws) ~ (ws ~> (
      stringExpression | numberExpression | booleanExpression | identifier | subexpression) <~ ws) ^^ {
      case lst ~ expr => Term(expr, lst) } 
    )

  def binaryExpression: Parser[List[LinePart]] = 
    (linePart <~ ws) ~  rep(ws ~> binaryOperator ~ (ws ~> linePart)) ^^ { 
      case lp0 ~ lst =>
	lp0 :: ( lst flatMap { case op ~ lp => List(BinaryOperator(op) , lp)} )
    }

  def expression: Parser[BaseExpression] = 
    (ws ~> binaryExpression <~ ws) ^^ { parseLineParts(_) match {
      case (t: Term) :: Nil => t.base
      case o => throw except("Expected list with one term, got: %s", o)()
    }
  }

  def parseLineParts(lst: List[LinePart], priority: Int = 0): List[LinePart] = {
    lst match {
      case Nil => throw except("List is empty")()
      case head :: Nil => lst

      // Parser operator
      case (arg0: Term) :: (op: BinaryOperator) ::  tail => {
	if (op.op.priority <= priority) {
	  lst
	} else if (tail isEmpty) {
	  List(Term(new ApplicationExpression(op.op, arg0.base)))
	} else {
	  parseLineParts(tail, op.op.priority) match {
	    case (arg1: Term) :: tail => parseLineParts(
	      Term(new ApplicationExpression(op.op, arg0.base, arg1.base)) :: tail, priority)
	    case o => throw except("Internal error, excepted list with term as start, got: %s", o)()
	  }
	}
      }

      // Parse function call
      case (arg0: Term) :: (arg1 : Term) :: tail => {
	val (terms, rest) = tail span { _.isInstanceOf[Term] }
	parseLineParts(Term(new ApplicationExpression(
	  arg0.base, (arg1.base :: (terms map { _.asInstanceOf[Term].base })) :_*)) :: rest, priority)
      }
    }
  }
}
  
