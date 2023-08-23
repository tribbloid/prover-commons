package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, NodeK, RewriterK}
import ai.acyclic.prover.commons.util.Summoner

trait Lawful {
  type Law_/\ <: Law

  type Node[v] = NodeK.Compat[Law_/\, v]

  type Rewriter[v] = RewriterK.Aux[Law_/\, v]
}

object Lawful {

  trait Bound[L <: Law] extends Lawful {
    type _Arrow <: Arrow
  }

  object Bound {

    // TODO: this can be made a pattern as an alternative match type
    implicit def impl[A <: Arrow]: Bound[Law.Impl[A]] { type _Arrow = A } =
      new Bound[Law.Impl[A]] {
        type _Arrow = A
      }

    def apply[L <: Law](
        implicit
        bound: Bound[_ >: L]
    ): bound.type = bound
  }

//  type BoundOf[L <: Law] = Bound[_ >: L]

  { // sanity
    val bounds = Summoner.summon[Bound[_ >: Law.Impl[Arrow.`~>`.^]]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  { // sanity
    trait Dummy extends Law.Impl[Arrow.`~>`.^] {}

    val bounds = Summoner.summon[Bound[_ >: Dummy]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  trait Struct[+L <: Law] {

    val law: L
    type _Arrow = law._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
