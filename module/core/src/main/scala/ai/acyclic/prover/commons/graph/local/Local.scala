package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine

import scala.language.implicitConversions
import ai.acyclic.prover.commons.graph.ops.{AnyGraphMixin, UpperSemilatticeMixin}

object Local extends Engine with AnyGraphMixin with UpperSemilatticeMixin {

  implicit class Batch[+V](
      val collect: Seq[V]
  ) extends IBatch[V] {
    override def isEmpty: Boolean = collect.isEmpty

    override def distinct: Batch[V] = collect.distinct

    override def flatMap[U](f: V => IterableOnce[U]): Batch[U] = collect.flatMap(f)

    override def union[V2 >: V](that: Batch[V2]): Batch[V2] = collect ++ that.collect
  }

  def parallelize[T](seq: Seq[T]): Batch[T] = Batch(seq)

  // TODO: add visualisation views
//  implicit def graphAsUnary[A <: Local.AnyGraph[?]](
//      prev: A
//  ): AnyGraphUnary[A] = {
//
//    AnyGraphUnary(prev)
//  }
//
//  implicit def outboundGraphAsUnary[A <: Local.AnyGraph.Outbound[?]](
//      prev: A
//  ): OutboundGraphUnary[A] = {
//
//    OutboundGraphUnary(prev)
//  }
//
//  implicit def upperSemilatticeAsUnary[A <: Local.Semilattice.Upper[?]](
//      prev: A
//  ): UpperSemilatticeUnary[A] = {
//
//    UpperSemilatticeUnary(prev)
//  }
//
//  private def compileTimeCheck[V](): Unit = {
//
//    implicitly[Tree[Int] <:< Semilattice.Upper[Int]]
//
//    implicitly[Tree[V] <:< Semilattice.Upper[V]]
//
//    implicitly[Local.Tree.axiom._Arrow <:< Arrow.OutboundT]
//
//    implicitly[Local.AnyGraph.Outbound.axiom._Arrow <:< Local.Tree.axiom._Arrow]
//
//    val example = Local.AnyGraph.Outbound.empty[Int]
//    implicitly[example._Arrow <:< Local.Tree.axiom._Arrow]
//  }
}
