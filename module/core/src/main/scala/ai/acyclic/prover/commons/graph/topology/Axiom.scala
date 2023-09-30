package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.util.Summoner

/**
  * a container of graph constraints
  */
trait Axiom {

  type _Arrow <: Arrow
}

object Axiom {

  def assume[X <: Axiom]: X = null.asInstanceOf[X]

  trait Impl[+A <: Arrow] extends Axiom { type _Arrow <: A }

  trait Match[X <: Axiom] extends Lawful { type _Arrow <: Arrow }

  object Match {

    implicit def onlyCase[A <: Arrow]: Match[Impl[A]] { type _Arrow = A } =
      new Match[Impl[A]] {
        override type _Arrow = A
      }
  }

  type MatchSub[L <: Axiom] = Match[_ >: L]

  { // sanity
    val bounds = Summoner.summon[MatchSub[Impl[Arrow.`~>`.^]]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  trait AnyGraphX extends Axiom.Impl[Arrow]

  object AnyGraphX extends Topology[AnyGraphX] {}

  trait NormalisedX extends AnyGraphX

  object NormalisedX extends Topology[AnyGraphX] {

    trait ForwardX extends NormalisedX with Axiom.Impl[Arrow.`~>`.^]

    object ForwardX extends Topology[ForwardX] {}
  }

  trait PosetX extends AnyGraphX
  object PosetX extends Topology[PosetX] {}

  trait SemilatticeX extends PosetX
  object SemilatticeX extends Topology[SemilatticeX] {

    trait UpperX extends SemilatticeX with NormalisedX.ForwardX
    object UpperX extends Topology[UpperX] {

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait TreeX extends SemilatticeX.UpperX
  object TreeX extends Topology[TreeX] {}

  private def sanity[V](): Unit = {

    implicitly[PosetX.Node[Int] <:< NormalisedX.Node[Int]]

    implicitly[PosetX.Node[V] <:< NormalisedX.Node[V]]
  }
}
