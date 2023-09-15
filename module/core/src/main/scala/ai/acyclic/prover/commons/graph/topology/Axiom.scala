package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.topology.Topology.HasTopology
import ai.acyclic.prover.commons.util.Summoner

/**
  * a container of graph constraints
  */
trait Axiom {

  type _Arrow <: Arrow
}

object Axiom {

  def apply[T <: Axiom]: T = null.asInstanceOf[T]

  trait Impl[+A <: Arrow] extends Axiom { type _Arrow <: A }

  trait Match[L <: Axiom] extends Lawful { type _Arrow <: Arrow }

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

  trait AnyGraphT extends Axiom.Impl[Arrow]

  object AnyGraphT extends HasTopology[AnyGraphT] {

    trait OutboundT extends AnyGraphT with Axiom.Impl[Arrow.`~>`.^]

    object OutboundT extends HasTopology[OutboundT] {}

  }

  trait PosetT extends AnyGraphT
  object PosetT extends HasTopology[PosetT] {}

  trait SemilatticeT extends PosetT
  object SemilatticeT extends HasTopology[SemilatticeT] {

    trait UpperT extends SemilatticeT with AnyGraphT.OutboundT
    object UpperT extends HasTopology[UpperT] {

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait TreeT extends SemilatticeT.UpperT
  object TreeT extends HasTopology[TreeT] {}

  private def sanity[V](): Unit = {

    implicitly[Topology[TreeT]]

    implicitly[PosetT.Node[Int] <:< AnyGraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< AnyGraphT.Node[V]]
  }
}
