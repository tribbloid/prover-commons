package org.shapesafe.graph.commons

/*
Scala implicit scope search order:

First look in current scope
Implicits defined in current scope
Explicit imports
wildcard imports
Same scope in other files
Now look at associated types in
Companion objects of a type
Implicit scope of an argument's type (2.9.1)
Implicit scope of type arguments (2.8.0)
Outer objects for nested types
Other dimensions
 */

/*
scala operator priority
LOWEST!
(all letters)
|
^
&
= !
< >
:
+ -
 * / %
(all other special characters)
HIGHEST!
 */

package object util {

//  val INDENT = "  "
//
//  def indent(text: String, str: String = INDENT): String = {
//    text.split('\n').filter(_.nonEmpty).map(str + _).mkString("\n")
//  }
}
