package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.util.Summoner

/**
  * a container of graph constraints
  */
trait Axiom {

  type _Arrow <: Arrow
}

object Axiom {

//  object ^ extends Axiom

  def apply[T <: Axiom]: T = null.asInstanceOf[T]

  trait Impl[+A <: Arrow] extends Axiom { type _Arrow <: A }

  trait MatchType[L <: Axiom] extends Lawful { type _Arrow <: Arrow }

  object MatchType {

    // TODO: this can be made a pattern as an alternative match type
    implicit def impl[A <: Arrow]: MatchType[Impl[A]] { type _Arrow = A } =
      new MatchType[Impl[A]] {
        override type _Arrow = A
      }
  }

  type Matching[L <: Axiom] = MatchType[_ >: L]

  { // sanity
    val bounds = Summoner.summon[Matching[Impl[Arrow.`~>`.^]]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }
}
