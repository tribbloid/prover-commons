package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.meta.{HasUniverse, Reflection}

trait HasReflection extends HasUniverse {

  val reflection: Reflection
  final override lazy val universe: reflection.universe.type = reflection.universe

  val format: TypeHierarchy
}
