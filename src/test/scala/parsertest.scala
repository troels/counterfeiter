package org.bifrost.counterfeiter.tests

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import java.io.File

import org.bifrost.counterfeiter.{ ExpressionParser, VariablePad, Expression,  HtmlOutput,
				                           BasicFunctions, HtmlTemplateParser, EmptyMachine, U,
				                           Counterfeiter, HtmlEscapedString }
import Expression.{ ElementaryExpression, UntypedExpression } 
import scala.util.parsing.input.CharSequenceReader

class SimpleTest extends FunSuite with ShouldMatchers { 
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
	  
  test("test parse negative number") {
    testExpressionParser[String]("-123") should equal("-123")
  }

  test("test parse complex expression") {
    testExpressionParser[String]("123 + 123 / 123") should equal("124")
    testExpressionParser[String]("true and false xor true") should equal("true")
    testExpressionParser[String]("not true and true") should equal("false")
    testExpressionParser[String]("true and not true") should equal("false")
    testExpressionParser[String]("1 + 2 * 2 = 5 and true and 5 = 1 + 2 * 2 and 4 != 1 + 2 * 2") should equal ("true")
    testExpressionParser[String]("1 - 1 = 0") should equal ("true")    
    testExpressionParser[String]("-2 = 1 + (1 - 4)") should equal ("true")
    testExpressionParser[String]("4 = 1 - (1 - 4) * 1 * -1 * -1") should equal ("true")
  } 

  test("test parse string") {
    testExpressionParser[String](""" "Hello I am me" """) should equal ("Hello I am me")
    testExpressionParser[String](""" "Hello" = "Hello" """) should equal ("true")
    testExpressionParser[String](""" "Hello\"there" """) should equal ("Hello\"there")
  }
    
  test("test basic functions") { 
    testExpressionParser[String](""" substring "123" 1 2 = "23" """) should equal ("true")
    testExpressionParser[String](""" substring (1 + 1) 0 1 = "2" """) should equal ("true")
  }

  test("test lists") { 
    testExpressionParser[String](""" [1,2,3, "a"] """) should equal ("""[1, 2, 3, a]""")
  }

  test("test map") { 
    testExpressionParser[String](""" {1: 2, "abc": 5, 2 + 2: 9  } """) should equal (
      """{"1": 2, "abc": 5, "4": 9}""")
  }

  test("test primitive templates") {
    testHtmlTemplateParser(
"""#hello h1(style="display: none")
 | HI THERE
""") should equal ("""<div id="hello"><h1 style="display: none">HI THERE</h1></div>""")
  }

  test("test tag in tag") {
    testHtmlTemplateParser(
"""a
 b
  c
""") should equal ("<a><b><c/></b></a>")
  }

  test("test expression") { 
    testHtmlTemplateParser(
""".hello_class.t2 h1
 | Hi there { 1 + 2 }{1+2}{332+4}
 | Where am I?
""") should equal ("""<div class="hello_class t2"><h1>Hi there 33336
Where am I?</h1></div>""")
  }

  test("test if") {
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

  test("test for") {
    testHtmlTemplateParser(
"""+ for a in [1,2,3,"a",  5]
 b(id={a})
  | {a}
""") should equal ("""<b id="1">1</b><b id="2">2</b><b id="3">3</b><b id="a">a</b><b id="5">5</b>""") }

  test("test parse template") { 
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

  test("test default arguments") { 
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

  test("test module parser") {
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

  test("test namespaces") { 
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

  test("test indexing lookup") { 
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

  test("Handling directory of templates") { 
    val machine = Counterfeiter.loadFromDir(
      new File(getClass.getClassLoader.getResource("templatetest").getFile))
  }
  
  test("htmlescaping") {
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

  test("Linecontinuation") {
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

  test("cssmode") {
    val mod = U.compileModule("""
namespace A

def test
 h1
  height: 10px; width: 10px;
  border: 1px solid; border-height: 10px; 
  h2
   text-color: blue; 
   | Hello there
""")
    
    mod.renderTemplate("A.test") should equal (
      """<h1 style="height: 10px; width: 10px; border: 1px solid; border-height: 10px"><h2 style="text-color: blue">Hello there</h2></h1>""")
  }

  test("Arbitrary object mode") { 
    val mod = U.compileModule("""
namespace A

def test ab
 h1
   | Hello there: {ab.cd.de}
   | 2 + 2 = { 2 + ab.cd.two }
""")

    case class De() { 
      override def toString = "Troels"
    }
    case class Cd(de: De) {
      val two = 2
    }
    case class Ab(cd: Cd) 
    
    val obj = Ab(Cd(De()))

    mod.renderTemplate("A.test", map=Map("ab" -> new UntypedExpression(obj))) should equal (
      """<h1>Hello there: Troels
2 + 2 = 4</h1>""")
  }

  test("test attributes") { 
    val str = """
namespace A

def test 
 h1  (a-b="hello \"there\"")
   | 2 + 2 = 4
"""

    val mod  = U.compileModule(str)
    mod.renderTemplate("A.test") should equal (
      """<h1 a-b="hello &quot;there&quot;">2 + 2 = 4</h1>""")
    }

  test("test list with hashes") { 
    val str = """
namespace A

def test 
 ul + for a in [{"b": "c", "d": 3}, 
             {"b": 45, "d": 10}]
  li(href={a["b"]}) | {a["d"]}
"""

    val mod  = U.compileModule(str)
    mod.renderTemplate("A.test") should equal (
      """<ul><li href="c">3</li><li href="45">10</li></ul>""")
    }

  test("test list from method and untypedexpression") { 
    val str = """
namespace A

def test arg
 ul + for a in arg.method
  li(href={a.link}) | {a.name}
"""
    
    case class ArgType(link: String, name:  String)
    case class ArgList(args: ArgType*) {
      def method: List[ArgType] = args toList
    }
    val mod  = U.compileModule(str)
    
    val arg = Map("arg" -> new UntypedExpression(ArgList(ArgType("hello", "there"), ArgType("goodbye", "again"))))
    mod.renderTemplate("A.test", map=arg)  should equal (
      """<ul><li href="hello">there</li><li href="goodbye">again</li></ul>""")
  }

  test("test is greater than") { 
    case class TestType(val arg: Int)
    val str = """
namespace A
    
def test arg
 + if arg.arg < 3
   | Hello there
 + else 
   | Hi there
"""
    
    val mod  = U.compileModule(str)

    val args = Map("arg" -> new UntypedExpression(TestType(2)))
    mod.renderTemplate("A.test", map=args) should equal("Hello there")
    val args2 = Map("arg" -> new UntypedExpression(TestType(4)))
    mod.renderTemplate("A.test", map=args2) should equal("Hi there")
  }

  test("for loop counter") { 
    case class TestType(val arg: Int)
    val str = """
namespace A
    
def test
 + for a in [1,2,3,4,5]
  + if for_loop_index_a >= 3
    | Hello
  + else
    | hi
"""
    
    val mod  = U.compileModule(str)

    mod.renderTemplate("A.test") should equal ("hihihiHelloHello")
  }

  test("boolean condition") { 
    case class TestType(val arg: Int)
    val str = """
namespace A
    
def test arg
 + if arg
  | Hello
 + else
  | Goodbye

def main
 - test { 1 < 2 }
 - test { 1 > 2 }
"""
    
    val mod  = U.compileModule(str)

    mod.renderTemplate("A.main") should equal ("Hello\nGoodbye")
  }
}
