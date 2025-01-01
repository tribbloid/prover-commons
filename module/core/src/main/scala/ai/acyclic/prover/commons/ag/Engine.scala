package ai.acyclic.prover.commons.ag

import ai.acyclic.prover.commons.ag.Foundation.{T, Template}

trait Engine extends Foundation.Engine {

  object Graph extends Foundation.Def {

    trait Prime[X <: Template] extends Foundation.Graph.Prime[X] {

      final override val engine: Engine.this.type = Engine.this
    }
  }
  type Graph[+I <: Topology.AnyGraph._I, +V] = Graph.Lt[I, V]

  implicit class TemplateView[T <: Foundation.Template](
      val self: T
  ) {

    trait Graph extends Graph.Prime[self.type] {

      final override val T: self.type = self
    }

    case class Unchecked(
        entries: _Batch[self._Node]
    ) extends Graph {}
  }

  case class On(
      topology: Topology
  ) {

    type _Graph[+V] = Graph[topology._I, V]

    def makeTight[I <: Topology.AnyGraph._I, V](
        nodes: _Batch[Foundation.Node[I, V]]
    )(
        implicit
        gt: Foundation.T[I, V]
    ): _Graph[V] = {

      val result: Graph.Prime[gt.type] = TemplateView[gt.type](gt).Unchecked(nodes)

      val r2: Graph[I, V] = result

      result
    }

    def makeExact[V](
        nodes: _Batch[topology.Node[V]]
    )(
        implicit
        gt: Foundation.T[topology._I, V]
    ): _Graph[V] = {

      val result: Graph.Prime[gt.type] = TemplateView[gt.type](gt).Unchecked(nodes)

      val r2: Graph[topology._I, V] = result

      result
    }
  }
}
