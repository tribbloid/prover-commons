package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.implicits.summon

/**
  * a container of graph constraints
  */
trait Axiom {

  type _Arrow <: Arrow
}

object Axiom {

  def assume[X <: Axiom]: X = null.asInstanceOf[X]

  trait Impl[+A <: Arrow] extends Axiom { type _Arrow <: A }

  trait ImplUnpack[X <: Axiom] extends Lawful { type _Arrow <: Arrow }

  object ImplUnpack {

    type Gt[L <: Axiom] = ImplUnpack[_ >: L]

    implicit def onlyCase[A <: Arrow]: ImplUnpack[Impl[A]] { type _Arrow = A } =
      new ImplUnpack[Impl[A]] {
        override type _Arrow = A
      }
  }

  { // sanity
    val bounds = summon.apply[ImplUnpack.Gt[Impl[Arrow.`~>`.^]]]

    implicitly[bounds._Arrow =:= Arrow.`~>`.^]
  }

  trait AnyGraphT extends Axiom.Impl[Arrow]

  object AnyGraphT extends Topology[AnyGraphT] {

    trait OutboundT extends AnyGraphT with Axiom.Impl[Arrow.`~>`.^]

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
