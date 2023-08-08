package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.util.Summoner

trait Topology extends Lawful {
  self: Singleton =>

  override type Law_/\ <: Law.AuxEx[Arrow]

  def witnessing[A <: Arrow, L <: Law_/\ with Law.AuxEx[A]](law: L): Law.WitnessImpl[A, L] =
    Law.WitnessImpl[A, L]()

  def witness[A <: Arrow, L <: Law_/\ with Law.AuxEx[A]]: Law.WitnessImpl[A, L] =
    Law.WitnessImpl[A, L]()

  // TODO: doesn't work, see SpikeHere
  implicit def summonWitness[A <: Arrow, L <: Law_/\ with Law.AuxEx[A]]: Law.WitnessImpl[A, L] =
    witness

  type Graph[V] = GraphK.Aux[Law_/\, V]

  implicit def summon: this.type = this
}

object Topology {

  object SpikeHere {

    {
      val w1 = Topology.TreeT.witness
    }

//    { // summon witness, doesn't work
//      val w1 = implicitly[Law.Witness[Topology.TreeT.Law_/\]] // (Topology.TreeT.summonWitness1)
//
//      val w2 = Summoner.summon[Law.Witness[Topology.TreeT.Law_/\]] // (Topology.TreeT.summonWitness1)
//    }

    { // ... explicitly, works
      val w1 = implicitly[Law.Witness[Topology.TreeT.Law_/\]](Topology.TreeT.witness)

      val w2 = Summoner.summon[Law.Witness[Topology.TreeT.Law_/\]](Topology.TreeT.witness)
    }
  }

  object AnyGraphT extends Topology {

    trait Law_/\ extends Law.AuxEx[Arrow]
//    lazy val ww = witnessing(new Law_/\ {})

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
