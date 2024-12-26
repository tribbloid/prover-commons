package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.util.{Erased, Summoner}

/**
  * a container of graph constraints
  */
trait Induction extends Erased {

  type _Arrow <: Arrow
}

object Induction {

  def assume[X <: Induction]: X = null.asInstanceOf[X]

  trait Lt_[+A <: Arrow] extends Induction { type _Arrow <: A }

//  trait ExtractArrow[X <: Induction] { type _Arrow <: Arrow }
//
//  object ExtractArrow {
//
//    type Gt[L <: Induction] = ExtractArrow[? >: L]
//
//    implicit def onlyCase[A <: Arrow]: ExtractArrow[Lt_[A]] { type _Arrow = A } =
//      new ExtractArrow[Lt_[A]] {
//        override type _Arrow = A
//      }
//  }

//  { // sanity
//    val bounds = Summoner.summon[ExtractArrow.Gt[Lt_[Arrow.OutboundT.^]]]
//
//    implicitly[bounds._Arrow =:= Arrow.OutboundT.^]
//  }

  object AnyGraphT extends Topology {

    type _Axiom = Induction

    object OutboundT extends Topology {

      trait _Axiom extends Induction {

        override type _Arrow <: Arrow.Outbound
      }
    }
  }

  object PosetT extends Topology {

    trait _Axiom extends Induction
  }

  object SemilatticeT extends Topology {

    trait _Axiom extends PosetT._Axiom

    object UpperT extends Topology {

      trait _Axiom extends SemilatticeT._Axiom with AnyGraphT.OutboundT._Axiom

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends Topology {

    trait _Axiom extends SemilatticeT.UpperT._Axiom

    case class Singleton[V](value: V) extends Node_[V] {

      final override lazy val induction: collection.immutable.Nil.type = Nil
    }

  }

  private def __sanity[V](): Unit = {

    implicitly[SemilatticeT.Node[Int] <:< PosetT.Node[Int]]

    implicitly[SemilatticeT.Node[V] <:< PosetT.Node[V]]
  }
}
