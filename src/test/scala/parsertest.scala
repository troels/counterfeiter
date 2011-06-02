package org.bifrost.counterfeiter.tests

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import java.io.File

@RunWith(classOf[JUnitRunner])
class SimpleTest extends Spec with ShouldMatchers { 
  import org.bifrost.counterfeiter.{ ExpressionParser, VariablePad, Expression,  HtmlOutput,
				     BasicFunctions, HtmlTemplateParser, EmptyMachine, U,
				     Counterfeiter, HtmlEscapedString }
  import Expression.ElementaryExpression
  import scala.util.parsing.input.CharSequenceReader

  def testExpressionParser[T](str: String)(implicit m: Manifest[T]): T = 
    ExpressionParser.expression(new CharSequenceReader(str)) match { 
      case ExpressionParser.Success(res, next) => 
	res eval EmptyMachine match {
	  case s: ElementaryExpression => s.extractOrThrow[T]
	  case other => fail(other.toString)
	}
      case o => fail(o.toString)
    }

  def testHtmlTemplateParser[T](in: String): String = 
    HtmlTemplateParser.htmlElem("")(new CharSequenceReader(in)) match {
      case HtmlTemplateParser.Success(res, next) => 
	res eval EmptyMachine
      case o => fail(o.toString)
    }

  def testHtmlTemplateDeclarationParser[T](in: String): HtmlOutput.HtmlTemplate = 
    HtmlTemplateParser.templateDeclaration("")(new CharSequenceReader(in)) match {
      case HtmlTemplateParser.Success(res, next) => res
      case o => fail(o.toString)
    }
	  
  it("test parse negative number") {
    testExpressionParser[String]("-123") should equal("-123")
  }

  it("test parse complex expression") {
    testExpressionParser[String]("123 + 123 / 123") should equal("124")
    testExpressionParser[String]("true and false xor true") should equal("true")
    testExpressionParser[String]("not true and true") should equal("false")
    testExpressionParser[String]("true and not true") should equal("false")
    testExpressionParser[String]("1 + 2 * 2 = 5 and true and 5 = 1 + 2 * 2 and 4 != 1 + 2 * 2") should equal ("true")
    testExpressionParser[String]("1 - 1 = 0") should equal ("true")    
    testExpressionParser[String]("-2 = 1 + (1 - 4)") should equal ("true")
    testExpressionParser[String]("4 = 1 - (1 - 4) * 1 * -1 * -1") should equal ("true")
  } 

  it("test parse string") {
    testExpressionParser[String](""" "Hello I am me" """) should equal ("Hello I am me")
    testExpressionParser[String](""" "Hello" = "Hello" """) should equal ("true")
    testExpressionParser[String](""" "Hello\"there" """) should equal ("Hello\"there")
  }
    
  it("test basic functions") { 
    testExpressionParser[String](""" substring "123" 1 2 = "23" """) should equal ("true")
    testExpressionParser[String](""" substring (1 + 1) 0 1 = "2" """) should equal ("true")
  }

  it("test lists") { 
    testExpressionParser[String](""" [1,2,3, "a"] """) should equal ("""[1, 2, 3, a]""")
  }

  it("test map") { 
    testExpressionParser[String](""" {1: 2, "abc": 5, 2 + 2: 9  } """) should equal (
      """{"1": 2, "abc": 5, "4": 9}""")
  }

  it("test primitive templates") {
    testHtmlTemplateParser(
"""#hello h1(style="display: none")
 | HI THERE
""") should equal ("""<div id="hello"><h1 style="display: none">HI THERE</h1></div>""")
  }

  it("test tag in tag") {
    testHtmlTemplateParser(
"""a
 b
  c
""") should equal ("<a><b><c/></b></a>")
  }

  it("test expression") { 
    testHtmlTemplateParser(
""".hello_class.t2 h1
 | Hi there { 1 + 2 }{1+2}{332+4}
 | Where am I?
""") should equal ("""<div class="hello_class t2"><h1>Hi there 33336
Where am I?</h1></div>""")
  }

  it("test if") {
   testHtmlTemplateParser(
"""#a
 + if 1 + 2 = 4
  h1
   | hello there
   span
    | go and die
   | hi there
 + else
  div
   | goodbye there
   span
    | go and die
   | hi there 
""") should equal ("""<div id="a"><div>goodbye there
<span>go and die</span>
hi there </div></div>""")
  }

  it("test for") {
    testHtmlTemplateParser(
"""+ for a in [1,2,3,"a",  5]
 b(id={a})
  | {a}
""") should equal ("""<b id="1">1</b><b id="2">2</b><b id="3">3</b><b id="a">a</b><b id="5">5</b>""") }

  it("test parse template") { 
    val tmpl = testHtmlTemplateDeclarationParser(
      """def a b c d
 + if b
  | {c}
 + else
  | {d}
""")


    tmpl.renderTemplate(
      EmptyMachine, List(
	Expression.falseExpression, Expression.trueExpression, Expression.falseExpression)) should equal ("false")

    tmpl.renderTemplate(
      EmptyMachine, 
      List(Expression.trueExpression),
      Map("c" -> new Expression.BasicExpression[String]("Hello world"),
	  "d" -> new Expression.BasicExpression[String]("Goodbye world"))) should equal ("Hello world")
			    
  }

  it("test default arguments") { 
    val tmpl = testHtmlTemplateDeclarationParser(
"""def a b={3 + 3} c={2} d
 e:
  span h
   | Hello there
 | Hello there {e} {b} {d}
""")

    tmpl.renderTemplate(EmptyMachine,
      Map("d" -> new Expression.BasicExpression[String]("hello"))) should equal("""Hello there <span><h>Hello there</h></span> 6 hello""")

    tmpl.renderTemplate(EmptyMachine,
      Map("d" -> new Expression.BasicExpression[String]("hi"), 
	  "e" -> new Expression.BasicExpression[String]("Something different"))
     ) should equal ("""Hello there Something different 6 hi""")
  }

  it("test module parser") {
    val module = U.compileModule("""
def tmpl1 name profession
 | Hello there {name}
 | You are an 
 h2 
  | { substring profession 0 3 }
 | Hi 

def tmpl2 name 
 profession: 
  span
   | Bicyclerepairman
 h2 
  | Hello { name }
  | you are not a { profession }

def tmpl3 name 
 - tmpl1 {name} {"Cyclist"}
 h1 span
  | Charming
 - tmpl2 
   name: 
    | Hello there
   profession:
    span
     | Killer
""")

    module.renderTemplate("tmpl2", map = Map("name" -> new Expression.BasicExpression[String]("hello"))
			) should equal ("<h2>Hello hello\nyou are not a <span>Bicyclerepairman</span></h2>")

    module.renderTemplate("tmpl3", List(new Expression.BasicExpression[String]("Hello"))) should equal (
      """Hello there Hello
You are an 
<h2>Cyc</h2>
Hi 
<h1><span>Charming</span></h1>
<h2>Hello Hello there
you are not a <span>Killer</span></h2>""")
  }

  it("test namespaces") { 
    val module = U.compileModule("""
namespace A

def tmpl1 name
 + if name = "Arne"
  | Hello there
 + else 
  | Hi
 - B.tmpl2 {name}

namespace B

def tmpl2 name
 | Hello {name}

def tmpl3 
 - tmpl2 {"Holger"}
""")
    
    module.renderTemplate("B.tmpl3", List()) should equal ("Hello Holger")
    
    module.renderTemplate("A.tmpl1", List(new Expression.BasicExpression[String]("Arne"))) should equal (
      """Hello there
Hello Arne""")
  }

  it("test indexing lookup") { 
    val mod = U.compileModule("""
namespace A

def mapTestTemplate
 | { ({ "hello": "there", "hi": {"hi": ["Been there"] } })["hi"]["hi"][0] }

def testTemplate
 | { [1,2,3,[1,2,3]][3][0] = 1 }

""")

    mod.renderTemplate("A.testTemplate", List()) should equal ("true")
    mod.renderTemplate("A.mapTestTemplate", List()) should equal ("Been there")
  }

  it("Handling directory of templates") { 
    val machine = Counterfeiter.loadFromDir(
      new File(getClass.getClassLoader.getResource("templatetest").getFile))
  }
  
  it("htmlescaping") {
    val mod = U.compileModule("""
namespace A

def test a b
 h1                              
  | { e a }
  | { "<Hi> &amp;there" }
  | { b }
 - smalltest
def smalltest
 h2 
  | Hi there
""")
    
    mod.renderTemplate("A.test", List(
      new Expression.BasicExpression[String]("<ab"), 
      new Expression.BasicExpression[HtmlEscapedString](HtmlEscapedString("&c")))) should equal(
"""<h1><ab
&lt;Hi&gt; &amp;amp;there
&c</h1>
<h2>Hi there</h2>""")
  }

  it("Linecontinuation") {
    val mod = U.compileModule("""
namespace A

def test
 h1 | Hi there
 h2 - testTmpl
  args0: | test

def testTmpl args0
 span | { args0 }
""")
    
    mod.renderTemplate("A.test") should equal(
"""<h1>Hi there</h1>
<h2><span>test</span></h2>""")
  }

}
