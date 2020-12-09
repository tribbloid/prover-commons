package com.tribbloids.graph.commons

package object util {

  type TypeTag[T] = ScalaReflection.universe.TypeTag[T]

  type Type = ScalaReflection.universe.Type

  val INDENT = "  "

  def indent(text: String, str: String = INDENT): String = {
    text.split('\n').filter(_.nonEmpty).map(str + _).mkString("\n")
  }
}
