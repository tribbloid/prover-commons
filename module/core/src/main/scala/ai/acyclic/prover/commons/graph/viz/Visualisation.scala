package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.{Delegating, HasInner}
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.topology.Axiom.Top
import ai.acyclic.prover.commons.graph.Engine.HasMaxRecursionDepth

object Visualisation extends HasInner { // TODO: should be a mixin

  abstract class Local[X <: Top](
      val applicableToType: Local.GraphType[X]
  ) extends Visualisation
      with HasMaxRecursionDepth {

    override type MaxGraph[V] = applicableToType.Graph[V]
    type MaxNode[V] = applicableToType.Node[V]

    def showNode[V](node: MaxNode[V]): Visual[V] = {

      val graph: applicableToType.Graph[V] = applicableToType.makeExact[V](node)
      val result: Visual[V] = show(graph)
      result
    }
  }
}

trait Visualisation {

  type MaxGraph[V] <: Local.AnyGraph[V]

  type Visual[V] = Delegating[MaxGraph[V]] {} // TODO: do we really need this?
  def show[V](data: MaxGraph[V]): Visual[V]
}
