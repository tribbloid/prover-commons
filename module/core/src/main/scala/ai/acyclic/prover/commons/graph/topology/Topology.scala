package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, NodeK}

abstract class Topology[L <: Law](
    implicit
    val matching: Lawful.Matching[L]
) extends Lawful {

  override type Law_/\ = L
  override type _Arrow = matching._Arrow

  implicit def _self: Topology[L] = this
}

object Topology {

  import Lawful.LawImpl

  trait AnyGraph extends LawImpl[Arrow]
  object AnyGraph extends Topology[AnyGraph] {

    trait Outbound extends AnyGraph with LawImpl[Arrow.`~>`.^]
    object Outbound extends Topology[Outbound]
  }

  trait Poset extends AnyGraph
  object Poset extends Topology[Poset]

  trait Semilattice extends Poset
  object Semilattice extends Topology[Semilattice] {

    trait Upper extends Semilattice with AnyGraph.Outbound
    object Upper extends Topology[Upper] {

      implicit class NodeOps[V](n: NodeK.Compat[Law_/\, V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait Tree extends Semilattice.Upper
  object Tree extends Topology[Tree]

  //  private def __sanity[V]: Unit = {
  //
  //    implicitly[Poset.StructKS[Int]#Node_~ <:< AnyGraph.StructKS[Int]#Node_~]
  //
  //    implicitly[Poset.StructKS[V]#Node_~ <:< AnyGraph.StructKS[V]#Node_~]
  //
  //    implicitly[Topology[Poset]] // can always summon topology
  //  }
}
