package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{GraphK, NodeK, RewriterK}
import ai.acyclic.prover.commons.graph.topology.Induction.*
import ai.acyclic.prover.commons.graph.topology.Lawful.Refined

abstract class Topology extends Lawful {
  self: Singleton =>

  override type _Axiom <: Induction
//  final type _Arrow = _Axiom#_Arrow

  type Graph[V] = GraphK.Aux[_Axiom, V]

  // TODO: agnostic to engine, move to topology
  trait Element extends Refined {
    // TODO: I don't think this trait should exist, Node and Rewriter should be agnostic to engines (local or distributed)
    //  Rewriter in addition should compile into e-graph

    override type _Axiom = Topology.this._Axiom
  }

  /**
    * 1st API, most universal
    * @tparam V
    *   value tyupe
    */
  trait Node_[V] extends NodeK.Aux_[_Axiom, V] with Element {}

  /**
    * 2nd API, all [[node]] under the same group can be connected to other [[node]]
    */
  trait Group {

    trait NodeInGroup extends NodeK.Untyped[_Axiom] with Element {
      self: Group.this.node =>

      type Value = Group.this.node
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
  trait Inspection[V] extends Lawful.Refined with Element {

    type Value = V

    type NodeV_ = Node_[V]

    //        type node <: _Node
    val inspect: V => NodeV

    implicit class ValueOps(v: V) {

      def asNode: NodeV = inspect(v)
    }
  }

  trait RewriterImpl[V] extends RewriterK.Aux_[_Axiom, V] with Element {}
}

object Topology {

  private def __sanity[V](): Unit = {
//
    implicitly[TreeT.Node[Int] <:< SemilatticeT.Node[Int]]

    implicitly[TreeT.Node[V] <:< SemilatticeT.Node[V]]

    implicitly[TreeT._Axiom <:< SemilatticeT._Axiom]
//
//    val example = Local.Tree.empty[Int]
//    implicitly[example._Axiom <:< Local.Tree._Axiom]
  }
}
