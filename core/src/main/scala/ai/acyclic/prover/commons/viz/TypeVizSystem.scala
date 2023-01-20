package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.reflect.{HasUniverse, Reflection}

trait TypeVizSystem extends HasUniverse {

  val reflection: Reflection
  final val universe: reflection.universe.type = reflection.universe

  val format: TypeVizFormat
}
