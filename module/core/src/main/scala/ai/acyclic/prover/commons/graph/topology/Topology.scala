package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Foundation.Structure
import ai.acyclic.prover.commons.graph.{Arrow, Foundation}
import ai.acyclic.prover.commons.graph.topology.Axiom.ExtractArrow
import ai.acyclic.prover.commons.graph.topology.Topology.{Impls, Lt}

abstract class Topology extends Foundation.Lawful {
  self: Singleton =>

  type _Graph[v] = Foundation.Graph.Lt[_Axiom, v]

  abstract class TopologicalMixin[T <: Topology, +V] extends Structure[_Axiom, V] {

    override val topology = Topology.this
  }

  trait Structure_[V] extends Foundation.Structure[_Axiom, V] {

    override val topology = Topology.this
  }

  trait Node_[V] extends Foundation.Node[_Axiom, V] with Structure_[V] {}

  trait Setter_[V] extends Foundation.Updater[_Axiom, V] with Structure_[V] {}

  /**
    * 2nd API, all [[node]] under the same group can be connected to other [[node]]
    */
  trait Codomain {

    trait Node_ extends Topology.this.Node_[node] {
      self: Codomain.this.node =>

      def value: node = this
    }

    type node <: Node_ // TODO: should be "FixedPoint"
  }

  trait Inspection[V] extends ai.acyclic.prover.commons.multiverse.CanInspect[V, Node_[V]] {}
}

object Topology {

  type Lt[+X <: Axiom.Top, +A <: Arrow] = Topology {

    type _Axiom <: X
    type _Arrow <: A
  }

  object AnyGraph extends Topology {

    trait _Axiom extends Axiom.Lt_[Arrow]
  }

  object Poset extends Topology {

    trait _Axiom extends AnyGraph._Axiom

    implicit class NodeOps[V](n: Node[V]) {

      def isLeaf: Boolean = n.inductions.isEmpty
    }
  }
}
