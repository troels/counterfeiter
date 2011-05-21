package org.bifrost.counterfeiter

import scala.collection.mutable.HashMap
import Expression.ElementaryExpression
import HtmlOutput.HtmlTemplate

abstract class Pad { 
  def lookup(identifier: String): Option[ElementaryExpression]
}

object EmptyPad extends Pad { 
  override def lookup(identifier: String) = None
  override def toString = "EmptyPad"
}
  
class VariablePad(vars: Map[String, ElementaryExpression] = Map(), parentPad: Pad = EmptyPad) extends Pad{ 
  override def lookup(identifier: String): Option[ElementaryExpression] =
    (vars get identifier) orElse (parentPad lookup identifier)

  override def toString = 
    "VariablePad: [content: %s\n, parent: %s]" format (vars toString, parentPad toString)
}

class Machine(val templates: Map[String, HtmlTemplate], val pad: Pad = EmptyPad) { 
  def this(templateList: List[HtmlTemplate], pad: Pad) = 
    this(templateList map { tmpl => tmpl.name -> tmpl } toMap, pad)

  def newPad(vars: Map[String, ElementaryExpression]) = 
    new Machine(templates, new VariablePad(vars, pad))

  def renderTemplate(name: String, lst: List[ElementaryExpression] = List(), 
		     map: Map[String, ElementaryExpression] = Map()) =
    templates(name).renderTemplate(this, lst, map)
}

object EmptyMachine extends Machine(Map[String, HtmlTemplate](), BasicFunctions.standardPad)

