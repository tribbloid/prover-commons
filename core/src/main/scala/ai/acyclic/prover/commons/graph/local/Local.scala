package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.Topology.GraphT

object Local extends LocalEngine.Syntax {

//  implicit def graphAsUnary[L <: Local.Graph._L, V](
//      self: LocalEngine._GraphKind.Aux[L, V]
//  ): GraphUnary.^[L, V] = {
//
//    val leaf = new LocalEngine.PlanKind.LeafPlan[L, V](self)
//
//    GraphUnary.^(leaf)
//  }
//
//  implicit def outboundGraphAsUnary[L <: Local.Graph.Outbound._L, V](
//      self: LocalEngine._GraphKind.Aux[L, V]
//  ): OutboundGraphUnary.^[L, V] = {
//
//    val leaf = new LocalEngine.PlanKind.LeafPlan[L, V](self)
//
//    OutboundGraphUnary.^(leaf)
//  }
//
//  implicit def upperSemilatticeAsUnary[L <: Local.Semilattice.Upper._L, V](
//      self: LocalEngine._GraphKind.Aux[L, V]
//  ): UpperSemilatticeUnary.^[L, V] = {
//
//    val leaf = new LocalEngine.PlanKind.LeafPlan[L, V](self)
//
//    UpperSemilatticeUnary.^(leaf)
//  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[Tree[Int] <:< Semilattice.Upper[Int]]

    implicitly[Tree[V] <:< Semilattice.Upper[V]]

    implicitly[Local.Tree.law._A <:< Arrow.`~>`.^]

    implicitly[Local.Graph.Outbound.law._A <:< Local.Tree.law._A]

    val example = Local.Graph.Outbound[Int]()
    implicitly[example.law._A <:< Local.Tree.law._A]
  }
}
