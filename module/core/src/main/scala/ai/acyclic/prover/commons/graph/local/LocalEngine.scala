package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.local.ops.{AnyGraphUnary, OutboundGraphUnary, UpperSemilatticeUnary}

import scala.language.implicitConversions

object LocalEngine extends Engine {

  final type Batch[+T] = Vector[T]
  def parallelize[T](seq: Seq[T]): Batch[T] = seq.toVector

  implicit def graphAsUnary[A <: Local.AnyGraph[?]](
      prev: A
  ): AnyGraphUnary.^[A] = {

    AnyGraphUnary.^(prev)
  }

  implicit def outboundGraphAsUnary[A <: Local.AnyGraph.Outbound[?]](
      prev: A
  ): OutboundGraphUnary.^[A] = {

    OutboundGraphUnary.^(prev)
  }

  implicit def upperSemilatticeAsUnary[A <: Local.Semilattice.Upper[?]](
      prev: A
  ): UpperSemilatticeUnary.^[A] = {

    UpperSemilatticeUnary.^(prev)
  }

}
