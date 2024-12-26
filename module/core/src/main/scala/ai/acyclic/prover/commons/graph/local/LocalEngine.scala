package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.local.ops.{AnyGraphUnary, OutboundGraphUnary, UpperSemilatticeUnary}

import scala.language.implicitConversions

object LocalEngine extends Engine {

  final type Batch[+T] = Vector[T]
  def parallelize[T](seq: Seq[T]): Batch[T] = seq.toVector

  implicit def graphAsUnary[A <: Local.AnyGraph.Graph[?]](
      self: A
  ): AnyGraphUnary.^[A] = {

    AnyGraphUnary.^(self)
  }

  implicit def outboundGraphAsUnary[A <: Local.AnyGraph.Outbound.Graph[?]](
      self: A
  ): OutboundGraphUnary.^[A] = {

    OutboundGraphUnary.^(self)
  }

  implicit def upperSemilatticeAsUnary[A <: Local.Semilattice.Upper.Graph[?]](
      self: A
  ): UpperSemilatticeUnary.^[A] = {

    UpperSemilatticeUnary.^(self)
  }

}
