package ai.acyclic.prover.commons.graph

trait Topology extends Lawful {
  self: Singleton =>

  override type Law_/\ <: Law.AuxEx[Arrow]

  implicit def summonWitness[A <: Arrow, L <: Law_/\ with Law.AuxEx[A]]: Law.WitnessA[A, L] =
    Law.WitnessA[A, L]()

  type Graph[V] = GraphK.Aux[Law_/\, V]

  implicit def summon: this.type = this
}

object Topology {

  object AnyGraphT extends Topology {

    trait Law_/\ extends Law.AuxEx[Arrow]

    object OutboundT extends Topology {

      trait Law_/\ extends AnyGraphT.Law_/\ with Law.AuxEx[Arrow.`~>`.^] {}
    }
  }

  object PosetT extends Topology {

    trait Law_/\ extends AnyGraphT.Law_/\
  }

  object SemilatticeT extends Topology {

    trait Law_/\ extends PosetT.Law_/\

    object UpperT extends Topology {

      trait Law_/\ extends SemilatticeT.Law_/\ with AnyGraphT.OutboundT.Law_/\

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends Topology {

    trait Law_/\ extends SemilatticeT.UpperT.Law_/\
  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< AnyGraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< AnyGraphT.Node[V]]
  }
}
