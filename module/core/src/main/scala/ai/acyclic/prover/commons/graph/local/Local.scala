package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine

import ai.acyclic.prover.commons.graph.ops.{AnyGraphMixin, PosetMixin, UpperSemilatticeMixin}

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

}
