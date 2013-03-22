counterfeiter
=============

A templating engine for programmers, aiming at finding just the right balance of power:
Versatile and full of proven features while avoiding injections, 
excessive code-in-templates and boilerplate.

This is an example template:

namespace Main

def main title statement
 html
  head 
   title | {title}
   - js "/js/js.js"
  body
   #main | {statement}

def js src
 script (type="text/javascript" src={file})
 
namespace Site

def frontpage title
 - Main.main 
  title: {title}
  statement: 
   #statement
    h1 | Welcome to Counterfeiter
    .text
      Enjoy yourself


The code can be called from Scala or Java, like:

Template.render("Site.frontpage", "Counterfeiter title")

and will result in html much like:
<!doctype html>
<html>
 <head>
  <title>Counterfeiter title</title>
  <script type="text/javascript" src="/js/js.js"></script>
 </head>
 <body>
  <div id="main">
   <h1>Welcome to counterfeiter</h1>
   <div class="text">
    Enjoy yourself
   </div>
  </div>
 </body>
</html>

All strings from outside (or inside) are html-escaped once. If they should not be escaped, 
their insertion must be preceded by an e like {e title}.

More.

There is a wide assortment of other features available, like let-statements, maps, lists, for-loops, 
if-statements, default arguments, arithmetic, boolean statements and many more features.
