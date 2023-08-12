package ai.acyclic.prover.commons.graph.law

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.util.Summoner

/**
  * a container of graph constraints
  */
trait Law[+A <: Arrow] extends Law.Arg {
  type _A <: A
}

object Law {

  trait Arg {

    type _A <: Arrow
  }

  trait ArgExtractor[L <: Law[_]] extends Arg {}
  object ArgExtractor {

    implicit def impl1[A <: Arrow]: ArgExtractor[Law[A]] { type _A = A } =
      new ArgExtractor[Law[A]] {
        override type _A = A
      }
  }

  type ArgOf[L <: Law[_]] = ArgExtractor[_ >: L]

  { // sanity
    val arrowOf = Summoner.summon[ArgOf[Law[Arrow.`~>`.^]]]

    implicitly[arrowOf._A =:= Arrow.`~>`.^]
  }

  trait AnyGraph extends Law[Arrow]
  object AnyGraph extends Topology[AnyGraph]

  trait
}
