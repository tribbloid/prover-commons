package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.plan.local.{GraphUnary, OutboundGraphUnary, UpperSemilatticeUnary}

import scala.language.implicitConversions

object LocalEngine extends Engine {

  final type Dataset[+T] = Vector[T]
  def parallelize[T](seq: Seq[T]): Dataset[T] = seq.toVector

//  private def compileTimeCheck[V](): Unit = {}

  implicit def graphAsUnary[L <: Local.Graph._L, V](
      self: LocalEngine.TheGraphKind.Aux[L, V]
  ): GraphUnary.^[L, V] = {

    val leaf = new LocalEngine.PlanKind.LeafPlan[L, V](self)

    GraphUnary.^(leaf)
  }

  implicit def outboundGraphAsUnary[L <: Local.Graph.Outbound._L, V](
      self: LocalEngine.TheGraphKind.Aux[L, V]
  ): OutboundGraphUnary.^[L, V] = {

    val leaf = new LocalEngine.PlanKind.LeafPlan[L, V](self)

    OutboundGraphUnary.^(leaf)
  }

  implicit def upperSemilatticeAsUnary[L <: Local.Semilattice.Upper._L, V](
      self: LocalEngine.TheGraphKind.Aux[L, V]
  ): UpperSemilatticeUnary.^[L, V] = {

    val leaf = new LocalEngine.PlanKind.LeafPlan[L, V](self)

    UpperSemilatticeUnary.^(leaf)
  }

}
