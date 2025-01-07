package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine

import ai.acyclic.prover.commons.graph.ops.{AnyGraphMixin, PosetMixin, UpperSemilatticeMixin}
import ai.acyclic.prover.commons.graph.topology.Axiom
import ai.acyclic.prover.commons.graph.viz.{Flow, Hierarchy, LinkedHierarchy}

object Local extends Engine with AnyGraphMixin with PosetMixin with UpperSemilatticeMixin {

  implicit class Batch[+V](
      val collect: Seq[V]
  ) extends IBatch[V] {
    override def isEmpty: Boolean = collect.isEmpty

    override def distinct: Batch[V] = collect.distinct

    override def flatMap[U](f: V => IterableOnce[U]): Batch[U] = collect.flatMap(f)

    override def union[V2 >: V](that: Batch[V2]): Batch[V2] = {
      collect ++ that.collect
    }
  }

  def parallelize[T](seq: Seq[T]): Batch[T] = Batch(seq)

  abstract class VisualOps[X <: Axiom.Top, V](
      graph: Graph[X, V],
      group: LinkedHierarchy#Group = LinkedHierarchy.Default.Group()
  ) {

    def text_flow(
        viz: Flow = Flow.Default
    ): viz.Viz[V] = {

      viz.show[V](graph)
    }

    def text_linkedHierarchy(
        group: LinkedHierarchy#Group = group,
        viz: LinkedHierarchy = LinkedHierarchy.Default
    )(
        implicit
        < : Graph[X, V] <:< Local.Diverging.Graph[V]
    ): viz.Visual[V] = {

      viz.show[V](graph)
    }

    def text_hierarchy(viz: Hierarchy = Hierarchy.Default)(
        implicit
        < : Graph[X, V] <:< Local.Diverging.UpperSemilattice[V]
    ): viz.Visual[V] = {

      viz.show[V](graph)
    }
  }
}
