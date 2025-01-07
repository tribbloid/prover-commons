package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.{Delegating, HasInner}
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.topology.Axiom.Top
import ai.acyclic.prover.commons.graph.Engine.HasMaxRecursionDepth
import ai.acyclic.prover.commons.graph.local.Local.Graph

object Visualisation extends HasInner { // TODO: should be a mixin

  abstract class Local[X <: Top](
      val applicableToType: Local.GraphType[X]
  ) extends Visualisation {

    override type MaxGraph[V] = applicableToType.Graph[V]
    type MaxNode[V] = applicableToType.Node[V]

    def showNode[V](node: MaxNode[V]): Visual[V] = {

      val graph: applicableToType.Graph[V] = applicableToType.makeExact[V](node)
      val result: Visual[V] = show(graph)
      result
    }
  }
}

trait Visualisation extends HasMaxRecursionDepth {

  type MaxGraph[V] <: Local.AnyGraph[V]

  trait Visual[V] extends Delegating[MaxGraph[V]] with HasMaxRecursionDepth {

    final lazy val maxRecursionDepth: Int = {
      unbox match {
        case Graph.Transforming(_, d) => d
        case _                        => HasMaxRecursionDepth.Default.maxRecursionDepth
      }
    }

  } // TODO: do we really need this?
  def show[V](data: MaxGraph[V]): Visual[V]
}
