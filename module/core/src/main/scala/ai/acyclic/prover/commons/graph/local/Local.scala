package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Arrow

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

    implicitly[Local.Tree.topology.LawImpl._A <:< Arrow.`~>`.^]

    implicitly[Local.AnyGraph.Outbound.topology.LawImpl._A <:< Local.Tree.topology.LawImpl._A]

    val example = Local.AnyGraph.Outbound.empty[Int]
    implicitly[example.law._A <:< Local.Tree.topology.LawImpl._A]
  }
}
