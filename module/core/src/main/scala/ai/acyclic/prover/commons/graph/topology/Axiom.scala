package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.util.{Erased, Summoner}

/**
  * a container of graph constraints
  */
trait Axiom extends Erased {

  type _Arrow <: Arrow
}

object Axiom {

  type Top = Topology.AnyGraphT._Axiom

  class Concrete[X <: Arrow]() extends Axiom {
    type _Arrow = X
  }

  def assume[X <: Axiom]: X = null.asInstanceOf[X]

  trait Lt_[+A <: Arrow] extends Axiom { type _Arrow <: A }

  trait ExtractArrow[X <: Axiom] { type _Arrow <: Arrow }

  object ExtractArrow {

    type Gt[L <: Axiom] = ExtractArrow[? >: L]

    implicit def onlyCase[A <: Arrow]: ExtractArrow[Lt_[A]] { type _Arrow = A } =
      new ExtractArrow[Lt_[A]] {
        override type _Arrow = A
      }
  }

  { // sanity
    val bounds = Summoner.summon[ExtractArrow.Gt[Lt_[Arrow.Outbound]]]

    implicitly[bounds._Arrow =:= Arrow.Outbound]
  }

  private def __sanity[V](): Unit = {

    implicitly[Topology.PosetT.Node[Int] <:< Topology.AnyGraphT.Node[Int]]

    implicitly[Topology.PosetT.Node[V] <:< Topology.AnyGraphT.Node[V]]
  }
}
