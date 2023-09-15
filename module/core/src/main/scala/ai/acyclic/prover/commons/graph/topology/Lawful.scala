package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, NodeK, RewriterK}
import ai.acyclic.prover.commons.util.Summoner

trait Lawful {
  type Conj_/\ <: Conj

  type Node[v] = NodeK.Compat[Conj_/\, v]

  type Rewriter[v] = RewriterK.Aux[Conj_/\, v]
}

object Lawful {

  trait Bound[L <: Conj] extends Lawful {
    type _Arrow <: Arrow
  }

  object Bound {

    // TODO: this can be made a pattern as an alternative match type
    implicit def impl[A <: Arrow]: Bound[Conj.Impl[A]] { type _Arrow = A } =
      new Bound[Conj.Impl[A]] {
        type _Arrow = A
      }

    def apply[L <: Conj](
        implicit
        bound: Bound[_ >: L]
    ): bound.type = bound
  }

//  type BoundOf[L <: Law] = Bound[_ >: L]

  { // sanity
    val bounds = Summoner.summon[Bound[_ >: Conj.Impl[Arrow.`~>`.^]]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  { // sanity
    trait Dummy extends Conj.Impl[Arrow.`~>`.^] {}

    val bounds = Summoner.summon[Bound[_ >: Dummy]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  trait Struct[+L <: Conj] {

    val assuming: L
    type _Arrow = assuming._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
