package ai.acyclic.prover.commons

package object reflect {

  val ScalaReflection: Reflection.Runtime.type = Reflection.Runtime

  val MacroReflection: Reflection.CompileTime.type = Reflection.CompileTime

  type RuntimeUniverse = scala.reflect.runtime.universe.type
  val RuntimeUniverse: RuntimeUniverse = scala.reflect.runtime.universe
}
