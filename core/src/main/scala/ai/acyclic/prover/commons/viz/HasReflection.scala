package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.reflect.{HasUniverse, Reflection}

trait HasReflection extends HasUniverse {

  val reflection: Reflection
  final val universe: reflection.universe.type = reflection.universe

  val format: TypeHierarchy
}
