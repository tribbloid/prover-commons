package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, NodeK}

abstract class Topology extends Lawful {

  implicit def _self: Topology.Aux[Law_/\] = this
}

object Topology {

  type Aux[L <: Law] = Topology { type Law_/\ = L }

  def apply[L <: Law]: Resolver[L] = Resolver()

//  abstract class Impl[L <: Law](
//      implicit
//      val matching: Lawful.Matching[L]
//  ) extends Topology {
//
//    override type Law_/\ = L
//
//    override type _Arrow = matching._Arrow
//  }

  case class Resolver[L <: Law]() {

    def resolve(
        implicit
        matching: Lawful.Matching[L]
    ): Topology.Aux[L] { type _Arrow = matching._Arrow } = new Topology {

      override type Law_/\ = L

      override type _Arrow = matching._Arrow
    }
  }

  def resolveImplicitly[L <: Law, M <: Lawful.Matching[L]](
      implicit
      matching: M
  ): Aux[L] { type _Arrow = matching._Arrow } = Resolver[L]().resolve

  import Lawful.LawImpl

  trait AnyGraph extends LawImpl[Arrow]
  object AnyGraph {

    trait Outbound extends AnyGraph with LawImpl[Arrow.`~>`.^]
//    object Outbound extends Impl[Outbound]
  }

  trait Poset extends AnyGraph

  trait Semilattice extends Poset
  object Semilattice {

    trait Upper extends Semilattice with AnyGraph.Outbound
    object Upper {

      implicit class NodeOps[V](n: NodeK.Compat[Upper, V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait Tree extends Semilattice.Upper

  { // sanity

    val Tree = Topology[Tree].resolve

    implicitly[Tree._Arrow <:< Arrow]

    implicitly[Tree._Arrow <:< Arrow.`~>`.^]
  }

  //  private def __sanity[V]: Unit = {
  //
  //    implicitly[Poset.StructKS[Int]#Node_~ <:< AnyGraph.StructKS[Int]#Node_~]
  //
  //    implicitly[Poset.StructKS[V]#Node_~ <:< AnyGraph.StructKS[V]#Node_~]
  //
  //    implicitly[Topology[Poset]] // can always summon topology
  //  }
}
