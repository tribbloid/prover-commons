package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, Foundation}
import ai.acyclic.prover.commons.graph.topology.Topology.AnyGraph

object DivergingForm {
  // graph of diverging normal form, all inductions are defined as outbound
  // can only have limited number of sources
  // most operations are only defined for diverging normal forms, other graphs has to be converted to such form first

  object Graph extends Topology {

    trait _Axiom extends AnyGraph._Axiom with Axiom.Lt_[Arrow.Outbound]
  }

  object Poset extends Topology {

    trait _Axiom extends Topology.Poset._Axiom with Graph._Axiom
  }

  object UpperSemilattice extends Topology {

    trait _Axiom extends Poset._Axiom {

      override def verify[X >: this.type <: Axiom.Top, V](graph: Foundation.Graph[X, V]): Unit = {
        require(graph.entries.collect.size <= 1, "not a upper semilattice")
      }
    }

  }

  object Tree extends Topology {

    trait _Axiom extends UpperSemilattice._Axiom
  }
}

// TODO: need converging
