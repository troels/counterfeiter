package org.bifrost.counterfeiter.tests

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class SimpleTest extends Spec with ShouldMatchers { 
  import org.bifrost.counterfeiter.Parser
  import scala.util.parsing.input.CharSequenceReader

  it("test basic expression") {
    val res = Parser.expression(new CharSequenceReader("hello"))
    println(res)
  }
}
