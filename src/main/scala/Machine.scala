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

class TemplateCollection(val templates: Map[String, HtmlTemplate]) {
  def this(templateList: List[HtmlTemplate]) = 
    this(templateList map { tmpl => tmpl.fullName -> tmpl } toMap)

  def get(name: String) = templates get name
  def getFromNamespace(namespace: String)(name: String) = get(U.joinNamespaceParts(namespace, name))
  def merge(tc: TemplateCollection) = new TemplateCollection(templates ++ tc.templates)
}
  
object EmptyTemplateCollection extends TemplateCollection(Map[String, HtmlTemplate]())
  
class Machine(val templates: TemplateCollection, val pad: Pad = EmptyPad, val namespace: String = "") { 
  def newPad(vars: Map[String, ElementaryExpression]) = 
    new Machine(templates, new VariablePad(vars, pad), namespace)
  
  def inNamespace(ns: String) = new Machine(templates, pad, ns)

  def renderTemplate(name: String, lst: List[ElementaryExpression] = List(), 
		     map: Map[String, ElementaryExpression] = Map()) = {
    (templates get name) orElse ((templates getFromNamespace namespace)(name)) match {
      case Some(tmpl) =>  tmpl renderTemplate (inNamespace(tmpl.namespace), lst, map)
      case None => throw new U.CounterFeiterException(
	      "Failed to lookup template: %s in namespace %s" format (name, namespace))
    }
  }
}

object EmptyMachine extends Machine(EmptyTemplateCollection, BasicFunctions.standardPad)
