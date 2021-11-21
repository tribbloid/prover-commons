package org.shapesafe.graph.commons.util

package object reflect {

  val ScalaReflection: Reflection.Runtime.type = Reflection.Runtime

  val MacroReflection: Reflection.CompileTime.type = Reflection.CompileTime
}
