package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.util.{Erased, Summoner}

/**
  * a container of graph constraints
  */
trait Axioms extends Erased {

  type _Arrow <: Arrow
}

object Axioms {

  def assume[X <: Axioms]: X = null.asInstanceOf[X]

  trait Lt_[+A <: Arrow] extends Axioms { type _Arrow <: A }

  trait ExtractArrow[X <: Axioms] { type _Arrow <: Arrow }

  object ExtractArrow {

    type Gt[L <: Axioms] = ExtractArrow[? >: L]

    implicit def onlyCase[A <: Arrow]: ExtractArrow[Lt_[A]] { type _Arrow = A } =
      new ExtractArrow[Lt_[A]] {
        override type _Arrow = A
      }
  }

  { // sanity
    val bounds = Summoner.summon[ExtractArrow.Gt[Lt_[Arrow.Outbound.^]]]

    implicitly[bounds._Arrow =:= Arrow.Outbound.^]
  }

  trait AnyGraphT extends Axioms.Lt_[Arrow]

  object AnyGraphT extends Topology[AnyGraphT] {

    trait OutboundT extends AnyGraphT with Axioms.Lt_[Arrow.Outbound.^]

    object OutboundT extends Topology[OutboundT] {}

  }

  trait PosetT extends AnyGraphT
  object PosetT extends Topology[PosetT] {}

  trait SemilatticeT extends PosetT
  object SemilatticeT extends Topology[SemilatticeT] {

    trait UpperT extends SemilatticeT with AnyGraphT.OutboundT
    object UpperT extends Topology[UpperT] {

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait TreeT extends SemilatticeT.UpperT
  object TreeT extends Topology[TreeT] {}

  private def __sanity[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< AnyGraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< AnyGraphT.Node[V]]
  }
}
