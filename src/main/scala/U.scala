package org.bifrost.counterfeiter

import java.lang.reflect.Method
import java.io.File

object U {
  import Implicits._
  class CounterFeiterException(msg: String) extends Exception(msg)

  def escapeHtml(str: String) =
    str.replaceAll("&", "&amp;").replaceAll(">", "&gt;")
        .replaceAll("<", "&lt;").replaceAll("\"", "&quot;")
        .replaceAll("'", "&#x27;")
  
  def isWord(str: String) = ("^\\w+$".r findPrefixOf str) isDefined

  def caseClassObjToMap(obj: AnyRef): Map[String, Any] = {
    val clazz = obj.getClass
    val fields = clazz.getDeclaredFields map { _ getName } toSet
    
    val methods = clazz.getMethods
    
    (methods foldLeft Map[String, Any]()) { 
      (map, method) => 
	      if ((fields contains method.getName) && (method.getTypeParameters.size == 0)) {
	        map + (method.getName -> method.invoke(obj))
	      } else {
	        map
	      }
    }
  }

  def intersperse[T, S <: T](lst: List[T], interElem: S): List[T] = {
    if ((lst isEmpty) || 1 == (lst size)) {
      lst
    } else {
      val lstr = (lst reverse)
      (lstr.tail foldLeft (List[T](lstr.head))) { 
	      (newLst, elem) => elem :: interElem :: newLst
      }
    }
  }
  
  class ListWrapper[T](lst: List[T]) {
    def intersperse[S <: T](interElem: S): List[T] = 
      U.intersperse(lst, interElem)
  }
  
  class OptionWrapper[T](v: Option[T]) {
    def getOrThrow(exc: => Exception) = 
      v match { 
	      case Some(ret) => ret
	      case None => throw exc
      }
  }
  
  class MapWrapper[A,B](map: Map[A,B]) {
    def getOrThrow(key: A, exc: => Exception) =
      map get key getOrThrow exc
  }

  object Implicits { 
    implicit def list2listWrapper[T](lst: List[T]): ListWrapper[T] = new ListWrapper[T](lst)
    implicit def option2optionWrapper[T](opt: Option[T]): OptionWrapper[T] = new OptionWrapper[T](opt)
    implicit def map2mapWrapper[A,B](map: Map[A,B]) = new MapWrapper[A,B](map)
  }

  def recursivelyGet(dir: File): List[File] = { 
    if (dir isDirectory)
      (dir listFiles) flatMap ( recursivelyGet(_) ) toList
    else
      List(dir)
  }
      
  def recursivelyGetWithExtension(dir: File, ext: String): List[File] = 
    recursivelyGet(dir) filter ( _.getName endsWith ext )

  def joinNamespaceParts(parts: String*) = 
    parts filter ( !_.isEmpty ) mkString "."
    
  def compileModule(code: String) = 
    new Machine(HtmlTemplateParser.parseModule(code), BasicFunctions.standardPad)
}
