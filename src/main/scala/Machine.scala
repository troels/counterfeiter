package org.bifrost.counterfeiter

import scala.collection.mutable.HashMap
import Expression.ElementaryExpression

abstract class Pad { 
  def lookup(identifier: String): Option[ElementaryExpression]
  def newPad(vars: Map[String, ElementaryExpression]): Pad
}

class VariablePad(vars: Map[String, ElementaryExpression], parentPad: Option[Pad]) extends Pad{ 
  def this(vars: Map[String, ElementaryExpression]) = this(vars, None)

  override def lookup(identifier: String): Option[ElementaryExpression] =
    (vars get identifier) orElse (parentPad flatMap (_ lookup identifier))
  override def newPad(vars: Map[String, ElementaryExpression]) = 
    new VariablePad(vars, Some(this))
}

class Machine { 
  private val templates = new HashMap[String, HtmlOutput.BaseElem]
  
//  private val globalPad = new 
}

