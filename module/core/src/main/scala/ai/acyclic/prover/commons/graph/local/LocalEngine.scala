package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.local.ops.{AnyGraphUnary, OutboundGraphUnary, UpperSemilatticeUnary}

import scala.language.implicitConversions

object LocalEngine extends Engine {

  final type Dataset[+T] = Vector[T]
  def parallelize[T](seq: Seq[T]): Dataset[T] = seq.toVector

  implicit def graphAsUnary[L <: Local.AnyGraph._Axiom, V](
      self: LocalEngine._GraphK.Aux[L, V]
  ): AnyGraphUnary.^[L, V] = {

    val leaf = self.asPlan

    AnyGraphUnary.^(leaf)
  }

  implicit def outboundGraphAsUnary[L <: Local.AnyGraph.Outbound._Axiom, V](
      self: LocalEngine._GraphK.Aux[L, V]
  ): OutboundGraphUnary.^[L, V] = {

    val leaf = self.asPlan

    OutboundGraphUnary.^(leaf)
  }

  implicit def upperSemilatticeAsUnary[L <: Local.Semilattice.Upper._Axiom, V](
      self: LocalEngine._GraphK.Aux[L, V]
  ): UpperSemilatticeUnary.^[L, V] = {

    val leaf = self.asPlan

    UpperSemilatticeUnary.^(leaf)
  }

}
