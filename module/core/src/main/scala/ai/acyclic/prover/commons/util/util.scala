package ai.acyclic.prover.commons

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

  // any 2 objects that has identical constrution ID must be constructed at the same time & allocated in the same memory
  // they are not just functionally identical, they have the same bare metal footprint!
  // a cloned object should absolutely has a different constructionID
//  def constructionID[T]: T => (Int, T) = ConstructionID.asInstanceOf[T => (Int, T)]
}
