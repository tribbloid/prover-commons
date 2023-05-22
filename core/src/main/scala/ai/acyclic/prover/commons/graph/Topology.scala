package ai.acyclic.prover.commons.graph

trait Topology extends Lawful {
  self: Singleton =>

  type Arrow_/\ <: Arrow

  override type Law_/\ <: Law { type _A <: Arrow_/\ }

  type LawImpl = Law_/\ { type _A = Arrow_/\ }
  def LawImpl: LawImpl

  trait LawImplMixin extends Law {
    override type _A = Arrow_/\
  }

  implicit def summonLaw: LawImpl = LawImpl

  type Graph[V] = GraphK.Aux[Law_/\, V]

  implicit def summon: this.type = this
}

object Topology {

  object AnyGraphT extends Topology {

    type Arrow_/\ = Arrow
    trait Law_/\ extends Law

    object LawImpl extends Law_/\ with LawImplMixin

    object OutboundT extends Topology {

      type Arrow_/\ = Arrow.`~>`.^
      trait Law_/\ extends AnyGraphT.Law_/\ {
        type _A <: Arrow.`~>`.^
      }

      object LawImpl extends Law_/\ with LawImplMixin
    }
  }

  object PosetT extends Topology {

    type Arrow_/\ = Arrow
    trait Law_/\ extends AnyGraphT.Law_/\

    object LawImpl extends Law_/\ with LawImplMixin
  }

  object SemilatticeT extends Topology {

    type Arrow_/\ = Arrow
    trait Law_/\ extends PosetT.Law_/\

    object LawImpl extends Law_/\ with LawImplMixin

    object UpperT extends Topology {

      type Arrow_/\ = Arrow.`~>`.^
      trait Law_/\ extends SemilatticeT.Law_/\ with AnyGraphT.OutboundT.Law_/\

      object LawImpl extends Law_/\ with LawImplMixin

      implicit class NodeOps[V](n: Node[V]) { // TODO: can be skipped if Law is fold into Node

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  object TreeT extends Topology {

    type Arrow_/\ = Arrow.`~>`.^
    trait Law_/\ extends SemilatticeT.UpperT.Law_/\

    override object LawImpl extends Law_/\ with LawImplMixin
  }

  private def compileTimeCheck[V](): Unit = {

    implicitly[PosetT.Node[Int] <:< AnyGraphT.Node[Int]]

    implicitly[PosetT.Node[V] <:< AnyGraphT.Node[V]]
  }
}
