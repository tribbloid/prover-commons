package ai.acyclic.prover.commons.graph.law

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.util.Summoner

trait Topology extends Lawful {}

object Topology {

  import ai.acyclic.prover.commons.graph.law.Topology.Law.LawImpl

  abstract class TopologyImpl[L <: Law](
      implicit
      val bounds: Law.BoundsOf[L]
  ) extends Lawful {

    type _A = bounds._Arrow

    type _Law = L
  }

  trait Law extends Lawful {
    type _Law = this.type
  }

  object Law {

    trait LawImpl[+A <: Arrow] extends Law {
      override type _Arrow <: A // TODO: remove
    }

    trait Bounds[L <: Law] extends Lawful {}
    object Bounds {

      // TODO: this can be made a pattern as an alternative match type
      implicit def impl[A <: Arrow]: Bounds[LawImpl[A]] { type _Arrow = A } =
        new Bounds[LawImpl[A]] {
          override type _Arrow = A
        }
    }
    type BoundsOf[L <: Law] = Bounds[_ >: L]

    { // sanity
      val bounds = Summoner.summon[BoundsOf[LawImpl[Arrow.`~>`.^]]]

      implicitly[bounds._Arrow =:= Arrow.`~>`.^]
    }
  }

  trait AnyGraph extends LawImpl[Arrow]
  object AnyGraph extends TopologyImpl[AnyGraph] {

    trait Outbound extends AnyGraph with LawImpl[Arrow.`~>`.^]
    object Outbound extends TopologyImpl[Outbound]
  }

  trait Poset extends AnyGraph
  object Poset extends TopologyImpl[Poset]

  trait Semilattice extends Poset
  object Semilattice extends TopologyImpl[Semilattice] {

    trait Upper extends Semilattice with AnyGraph.Outbound
    object Upper extends TopologyImpl[Upper] {

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait Tree extends Semilattice.Upper
  object Tree extends TopologyImpl[Tree]

  private def __sanity[V]: Unit = {

    implicitly[Poset.Node[Int] <:< AnyGraph.Node[Int]]

    implicitly[Poset.Node[V] <:< AnyGraph.Node[V]]
  }
}
