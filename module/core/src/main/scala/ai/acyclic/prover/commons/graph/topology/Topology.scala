package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, Foundation}
import ai.acyclic.prover.commons.graph.topology.Axiom.ExtractArrow
import ai.acyclic.prover.commons.graph.topology.Topology.AnyGraphT._Axiom
import ai.acyclic.prover.commons.graph.topology.Topology.SemilatticeT._Axiom
import ai.acyclic.prover.commons.util.Erased

abstract class Topology extends Foundation.Lawful {
  self: Singleton =>

  type _Graph[v] = Foundation.GraphK.Lt[_Axiom, v]
//
  implicit def concreteAxiom(
      implicit
      extractArrow: ExtractArrow.Gt[_Axiom] // TODO: how to remove this crap?
  ): Axiom.Concrete[extractArrow._Arrow] & _Axiom = Erased.apply[Axiom.Concrete[extractArrow._Arrow] & _Axiom]

  def reify(
      implicit
      extractArrow: ExtractArrow.Gt[_Axiom]
  ): Topology.Impls[_Axiom, extractArrow._Arrow] =
    Topology.Impls[_Axiom, extractArrow._Arrow](this)(concreteAxiom)
}

object Topology {

//  implicit def reifyConvert[T <: Topology](t: T)(
//      implicit
//      extractArrow: ExtractArrow.Gt[t._Axiom] // TODO: how to remove this crap?
//  ): Reify[t._Axiom, extractArrow._Arrow] = Topology.Reify(t)(Axiom.Concrete[extractArrow._Arrow])

  case class Impls[
      X <: Axiom.Top,
      A <: Arrow
  ](val topology: Topology { type _Axiom = X })(
      val concreteAxiom: Axiom.Concrete[A] with X
  ) {

    trait _Structure extends Foundation.Structure[Axiom.Concrete[A] with X] {

      override val axiom = Impls.this.concreteAxiom
    }

    trait Node_[V] extends Foundation.NodeK.Aux_[X, V] with _Structure {}

    trait Setter_[V] extends Foundation.Setter.Aux_[X, V] with _Structure {}

    /**
      * 2nd API, all [[node]] under the same group can be connected to other [[node]]
      */
    trait NodeGroup {

      trait NodeInGroup extends Node_[node] {
        self: NodeGroup.this.node =>

        def value = this
      }

      type node <: NodeInGroup
    }

    /**
      * 3rd API, define a [[inspect]] constructor that works on every [[V]]
      *
      * implicit function allows [[inspect]] to act as an extension of [[V]]
      *
      * @tparam V
      *   value type
      */
    trait Inspection[V] {

      type Node_ = Impls.this.Node_[V]

      //        type node <: _Node
      val inspect: V => Node_

      implicit def asNode(v: V): Node_ = inspect(v)
    }

  }

  object AnyGraphT extends Topology {

    trait _Axiom extends Axiom.Lt_[Arrow]

    object OutboundT extends Topology {

      trait _Axiom extends AnyGraphT._Axiom with Axiom.Lt_[Arrow.Outbound]
    }
  }

  object PosetT extends Topology {

    trait _Axiom extends AnyGraphT._Axiom
  }

  object SemilatticeT extends Topology {

    trait _Axiom extends PosetT._Axiom

    object UpperT extends Topology {

      trait _Axiom extends SemilatticeT.this._Axiom with AnyGraphT.OutboundT._Axiom

      implicit class NodeOps[V](n: Node[V]) {

        def isLeaf: Boolean = n.inductions.isEmpty
      }
    }
  }

  object TreeT extends Topology {

    trait _Axiom extends SemilatticeT.UpperT._Axiom
  }

  {
    // sanity
    implicitly[Topology.SemilatticeT.UpperT._Axiom]
  }
}
