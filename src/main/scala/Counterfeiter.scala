package org.bifrost.counterfeiter

import java.io.File
import com.google.common.io.Files
import java.nio.charset.Charset

object Counterfeiter {
  def createCounterfeiter(files: List[File]): Machine =  { 
    val tc = ((files toStream) map { 
      f => Files toString (f, Charset.forName("UTF-8"))
    } foldLeft (EmptyTemplateCollection: TemplateCollection)) { 
      (accum, text) => accum merge (HtmlTemplateParser.parseModule(text))
    }

    new Machine(tc, BasicFunctions.standardPad)
  }

  def loadFromDir(dir: File, ext: String = ".cf"): Machine = 
    createCounterfeiter(U.recursivelyGetWithExtension(dir, ext))
}
