package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.topology.Topology.AnyGraph

object DivergingForm {
  // graph of diverging normal form, all inductions are defined as outbound
  // can only have limited number of sources
  // most operations are only defined for diverging normal forms, other graphs has to be converted to such form first

  object Graph extends Topology {

    trait _Axiom extends AnyGraph._Axiom

    type _Arrow = Arrow.Outbound

    override def mkArrow(text: Option[String]): _Arrow = {
      text match {
        case Some(tt) => Arrow.Outbound.OfText(Some(tt))
        case None     => Arrow.Outbound
      }
    }
  }

  object Poset extends Topology {

    trait _Axiom extends Topology.Poset._Axiom with Graph._Axiom

    type _Arrow = Arrow.Outbound

    override def mkArrow(text: Option[String]): _Arrow = {
      text match {
        case Some(tt) => Arrow.Outbound.OfText(Some(tt))
        case None     => Arrow.Outbound
      }
    }
  }

  object UpperSemilattice extends Topology {

    trait _Axiom extends Poset._Axiom {}

    type _Arrow = Arrow.Outbound

    override def mkArrow(text: Option[String]): _Arrow = {
      text match {
        case Some(tt) => Arrow.Outbound.OfText(Some(tt))
        case None     => Arrow.Outbound
      }
    }

  }

  object Tree extends Topology {

    trait _Axiom extends UpperSemilattice._Axiom

    type _Arrow = Arrow.Outbound

    override def mkArrow(text: Option[String]): _Arrow = {
      text match {
        case Some(tt) => Arrow.Outbound.OfText(Some(tt))
        case None     => Arrow.Outbound
      }
    }
  }
}

// TODO: need converging
