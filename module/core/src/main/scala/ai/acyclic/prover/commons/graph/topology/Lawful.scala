package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, NodeK, RewriterK}
import ai.acyclic.prover.commons.util.Summoner

trait Lawful {

  type Law_/\ <: Law
  type _Arrow <: Arrow // TODO: remove

  type Node[v] = NodeK.Compat[Law_/\, v]

  type Rewriter[v] = RewriterK.Aux[Law_/\, v]

  //  final class StructKS[V] extends Struct {
  //
  //    type Value = V
  //
  //    override type _Law = Lawful.this._Law
  //    override type _Arrow = Lawful.this._Arrow
  //  }
}

object Lawful {

  trait LawImpl[+A <: Arrow] extends Lawful { // mark for removal
    type _Law = this.type
    override type _Arrow <: A // TODO: remove
  }

  trait MatchType[L <: Law] extends Lawful {}

  object MatchType {

    // TODO: this can be made a pattern as an alternative match type
    implicit def impl[A <: Arrow]: MatchType[LawImpl[A]] { type _Arrow = A } =
      new MatchType[LawImpl[A]] {
        override type _Arrow = A
      }
  }

  type Matching[L <: Law] = MatchType[_ >: L]

  { // sanity
    val bounds = Summoner.summon[Matching[LawImpl[Arrow.`~>`.^]]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  { // sanity
    val bounds = Summoner.summon[Matching[Topology.Tree]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  trait Struct[+L <: Law] extends Lawful {

//    val ev: Topology.Ev[_ <: L]

//    final val topology = ev.topology
//
//    override type Law_/\ = ev.Law_/\
//
//    override type _Arrow = ev._Arrow

    type Value // bound type of values of this node and all its descendants, NOT the type of this value!
  }
}
