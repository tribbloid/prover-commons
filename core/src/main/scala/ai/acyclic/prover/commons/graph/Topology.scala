package ai.acyclic.prover.commons.graph

trait Topology extends Lawful {
  self: Singleton =>

  type Arrow_/\ <: Arrow

  override type Law_/\ <: Law { type _A <: Arrow_/\ }

  trait LawImpl extends Law {
    type _A = Arrow_/\
  }

  type Graph[V] = GraphK.Aux[Law_/\, V]

  implicit def summon: this.type = this
}

object Topology {

  object AnyGraphT extends Topology {

    type Arrow_/\ = Arrow
    trait Law_/\ extends Law

    object OutboundT extends Topology {

      type Arrow_/\ = Arrow.`~>`.^
      trait Law_/\ extends AnyGraphT.Law_/\ {
        type _A <: Arrow.`~>`.^
      }
    }
  }

  object PosetT extends Topology {

    type Arrow_/\ = Arrow
    trait Law_/\ extends AnyGraphT.Law_/\
  }

  object SemilatticeT extends Topology {

    type Arrow_/\ = Arrow
    trait Law_/\ extends PosetT.Law_/\

    object UpperT extends Topology {

      type Arrow_/\ = Arrow.`~>`.^
      trait Law_/\ extends SemilatticeT.Law_/\ with AnyGraphT.OutboundT.Law_/\

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends Topology {

    type Arrow_/\ = Arrow.`~>`.^
    trait Law_/\ extends SemilatticeT.UpperT.Law_/\

  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< AnyGraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< AnyGraphT.Node[V]]
  }
}
