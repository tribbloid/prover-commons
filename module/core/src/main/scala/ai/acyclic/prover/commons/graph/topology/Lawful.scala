package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, NodeK, RewriterK}
import ai.acyclic.prover.commons.util.Summoner

trait Lawful {
  type _Axiom <: Axiom

  type Node[v] = NodeK.Compat[_Axiom, v]

  type Rewriter[v] = RewriterK.Aux[_Axiom, v]

}

object Lawful {

  trait Bound[X <: Axiom] extends Lawful {
    type _Arrow <: Arrow
  }

  object Bound {

    // TODO: this can be made a pattern as an alternative match type
    implicit def impl[A <: Arrow]: Bound[Axiom.Impl[A]] { type _Arrow = A } =
      new Bound[Axiom.Impl[A]] {
        type _Arrow = A
      }

    def apply[X <: Axiom](
        implicit
        bound: Bound[_ >: X]
    ): bound.type = bound
  }

//  type BoundOf[L <: Law] = Bound[_ >: L]

  { // sanity
    val bounds = Summoner.summon[Bound[_ >: Axiom.Impl[Arrow.`~>`.^]]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  { // sanity
    trait Dummy extends Axiom.Impl[Arrow.`~>`.^] {}

    val bounds = Summoner.summon[Bound[_ >: Dummy]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  trait Struct[+X <: Axiom] extends Lawful {

    override type _Axiom <: X

    val assuming: X
    type _Arrow = assuming._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
