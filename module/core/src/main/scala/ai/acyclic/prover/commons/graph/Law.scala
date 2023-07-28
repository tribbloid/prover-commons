package ai.acyclic.prover.commons.graph

/**
  * a container of graph constraints
  */
trait Law {

  type _A <: Arrow
}

object Law {

  trait AuxEx[+A <: Arrow] extends Law { type _A <: A }

  trait Witness[L <: AuxEx[Arrow]]

  case class WitnessA[A <: Arrow, L <: AuxEx[A]]() extends Witness[L] {

    type _A = A
  }
}
