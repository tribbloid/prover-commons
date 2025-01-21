package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, Foundation}
import ai.acyclic.prover.commons.util.{Erased, Summoner}

/**
  * a container of graph constraints
  */
trait Axiom extends Erased {

  type _Arrow <: Arrow

  def verify[X >: this.type <: Axiom.Top, V](graph: Foundation.Graph[X, V]): Unit = {
    // by default, do nothing
    // TODO: may have diamond subtyping problem
  }
}

object Axiom {

  type Top = Topology.AnyGraph._Axiom

  class Reify[X <: Arrow]() extends Axiom {
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

}
