package org.bifrost.counterfeiter
import java.lang.reflect.Method

object U {
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

  object Implicits { 
    implicit def list2listWrapper[T](lst: List[T]): ListWrapper[T] = new ListWrapper[T](lst)
  }
    
}
