package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.local.ops.{AnyGraphUnary, ForwardUnary, UpperSemilatticeUnary}

import scala.language.implicitConversions

object LocalEngine extends Engine {

  final type Dataset[+T] = Vector[T]
  def parallelize[T](seq: Seq[T]): Dataset[T] = seq.toVector

  implicit class GraphView[L <: Local.AnyGraph._Axiom, V](
      self: LocalEngine._GraphK.Compat[L, V]
  ) {

    lazy val asPlan = new PlanK.LeafPlan[L, V](self)
  }

  implicit def graphAsUnary[L <: Local.AnyGraph._Axiom, V](
      self: LocalEngine._GraphK.Compat[L, V]
  ): AnyGraphUnary.^[L, V] = {

    val leaf = self.asPlan

    AnyGraphUnary.^(leaf)
  }

  implicit def outboundGraphAsUnary[L <: Local.AnyGraph.Forward._Axiom, V](
      self: LocalEngine._GraphK.Compat[L, V]
  ): ForwardUnary.^[L, V] = {

    val leaf = self.asPlan

    ForwardUnary.^(leaf)
  }

  implicit def upperSemilatticeAsUnary[L <: Local.Semilattice.Upper._Axiom, V](
      self: LocalEngine._GraphK.Compat[L, V]
  ): UpperSemilatticeUnary.^[L, V] = {

    val leaf = self.asPlan

    UpperSemilatticeUnary.^(leaf)
  }

}
