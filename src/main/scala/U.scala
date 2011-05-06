package org.bifrost.counterfeiter

object U {
  class CounterFeiterException(msg: String) extends Exception

  def escapeHtml(str: String) =
    str.replaceAll("&", "&amp;").replaceAll(">", "&gt;")
        .replaceAll("<", "&lt;").replaceAll("\"", "&quot;")
        .replaceAll("'", "&#x27;")
  
  def isWord(str: String) = ("^\\w+$".r findPrefixOf str) isDefined
}
