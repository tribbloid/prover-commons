package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, Foundation}
import ai.acyclic.prover.commons.graph.topology.Axiom.ExtractArrow
import ai.acyclic.prover.commons.util.Phantom

import scala.language.implicitConversions

abstract class Topology extends Foundation.Lawful {
  self: Singleton =>

  type _Graph[v] = Foundation.Graph.Lt[_Axiom, v]

  implicit def reifyAxiom(
      implicit
      extractArrow: ExtractArrow.Gt[_Axiom] // TODO: how to remove this crap?
  ): Axiom.Reify[extractArrow._Arrow] & _Axiom = Phantom.apply[Axiom.Reify[extractArrow._Arrow] & _Axiom]()

  def reify(
      implicit
      extractArrow: ExtractArrow.Gt[_Axiom]
  ): Topology.Impls[_Axiom, extractArrow._Arrow] =
    Topology.Impls[_Axiom, extractArrow._Arrow](this)(reifyAxiom)
}

object Topology {

  case class Impls[
      X <: Axiom.Top,
      A <: Arrow
  ](topology: Topology { type _Axiom = X })(
      val concreteAxiom: Axiom.Reify[A] & X
  ) {

    trait _Structure[V] extends Foundation.Structure[Axiom.Reify[A] & X, V] {

      override val axiom: Axiom.Reify[A] & X = Impls.this.concreteAxiom
    }

    trait Node_[V] extends Foundation.Node[X, V] with _Structure[V] {}

    trait Setter_[V] extends Foundation.Setter[X, V] with _Structure[V] {}

    /**
      * 2nd API, all [[node]] under the same group can be connected to other [[node]]
      */
    trait Codomain {

      trait Node_ extends Impls.this.Node_[node] {
        self: Codomain.this.node =>

        def value: node = this
      }

      type node <: Node_ // TODO: should be "FixedPoint"
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
      implicit def asNodes(vs: Seq[V]): Seq[Node_] = vs.map(inspect)
    }

  }

  object AnyGraph extends Topology {

    trait _Axiom extends Axiom.Lt_[Arrow]
  }

  object Poset extends Topology {

    trait _Axiom extends AnyGraph._Axiom

    implicit class NodeOps[V](n: Node[V]) {

      def isLeaf: Boolean = n.inductions.isEmpty
    }
  }
}
