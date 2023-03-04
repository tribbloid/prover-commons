package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.local.ops.{AnyGraphUnary, OutboundGraphUnary, UpperSemilatticeUnary}

import scala.language.implicitConversions

object LocalEngine extends Engine {

  final type Dataset[+T] = Vector[T]
  def parallelize[T](seq: Seq[T]): Dataset[T] = seq.toVector

  implicit def graphAsUnary[L <: Local.AnyGraph.Law_/\, V](
      self: LocalEngine.TheGraphK.Aux[L, V]
  ): AnyGraphUnary.^[L, V] = {

    val leaf = new LocalEngine.PlanK.LeafPlan[L, V](self)

    AnyGraphUnary.^(leaf)
  }

  implicit def outboundGraphAsUnary[L <: Local.AnyGraph.Outbound.Law_/\, V](
      self: LocalEngine.TheGraphK.Aux[L, V]
  ): OutboundGraphUnary.^[L, V] = {

    val leaf = new LocalEngine.PlanK.LeafPlan[L, V](self)

    OutboundGraphUnary.^(leaf)
  }

  implicit def upperSemilatticeAsUnary[L <: Local.Semilattice.Upper.Law_/\, V](
      self: LocalEngine.TheGraphK.Aux[L, V]
  ): UpperSemilatticeUnary.^[L, V] = {

    val leaf = new LocalEngine.PlanK.LeafPlan[L, V](self)

    UpperSemilatticeUnary.^(leaf)
  }

}
