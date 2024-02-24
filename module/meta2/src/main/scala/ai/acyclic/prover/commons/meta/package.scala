package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.refl.Reflection

package object meta {

  val ScalaReflection: Reflection.Runtime.type = Reflection.Runtime
//  val MacroReflection: Reflection.CompileTime.type = Reflection.CompileTime

  type RuntimeUniverse = scala.reflect.runtime.universe.type
  val RuntimeUniverse: RuntimeUniverse = scala.reflect.runtime.universe
}
