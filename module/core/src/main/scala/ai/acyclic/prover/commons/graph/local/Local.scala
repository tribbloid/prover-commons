package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine

import scala.language.implicitConversions
import ai.acyclic.prover.commons.graph.ops.{AnyGraphMixin, UpperSemilatticeMixin}
import ai.acyclic.prover.commons.graph.topology.Axiom
import ai.acyclic.prover.commons.graph.viz.{Flow, Hierarchy, LinkedHierarchy}

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

  abstract class Show[X <: Axiom.Top, V](graph: Graph.Lt[X, V]) {

    def text_flow(
        viz: Flow = Flow.Default
    ): viz.Viz[V] = {

      viz.show[V](graph)
    }

    def text_linkedHierarchy(
        group: LinkedHierarchy#Group = LinkedHierarchy.Default.Group(),
        viz: LinkedHierarchy = LinkedHierarchy.Default
    )(
        implicit
        < : Graph.Lt[X, V] <:< Local.AnyGraph.Outbound[V]
    ) = {

      viz.show[V](graph)
    }

    def text_hierarchy(viz: Hierarchy = Hierarchy.Default)(
        implicit
        < : Graph.Lt[X, V] <:< Local.Semilattice.Upper[V]
    ) = {

      viz.show[V](graph)
    }
  }

  implicit class showGraph[X <: Axiom.Top, V](graph: Graph.Lt[X, V]) extends Show[X, V](graph) {}

  implicit class showNode[X <: Axiom.Top, V](node: Node[X, V])(
      implicit
      assuming: X
  ) extends Show[X, V](
        Local[X, V](node)
      ) {}
}
