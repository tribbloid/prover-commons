package ai.acyclic.prover.commons.graph.topology

import ai.acyclic.prover.commons.graph.{Arrow, NodeK}

class Topology[L <: Law]() extends Lawful {

  type Law_/\ = L

  implicitly[Law_/\ <:< Law]
}

object Topology {

  def get[L <: Law](
      implicit
      matching: Lawful.Matching[L]
  ): Topology[L] { type _Arrow = matching._Arrow } = new Topology[L] {

    final override type _Arrow = matching._Arrow
  }

  class Get[L <: Law](
      implicit
      matching: Lawful.Matching[L]
  ) extends Topology[L] {

    final override type _Arrow = matching._Arrow
  }

//  class EvM[L <: Law, M <: Lawful.Matching[L]]()(
//      implicit
//      val matching: M
//  ) extends Lawful {
//
//    final val topology = Topology.this
//
//    override type Law_/\ = L
//
//    override type _Arrow = matching._Arrow
//  }
//
//  type Ev[L <: Law] = EvM[L, _]
//
//  def ev[L <: Law](
//      implicit
//      matching: Lawful.Matching[L]
//  ) = new EvM[L, matching.type]()(matching)
//
//  def dummy1[L <: Law, M <: Lawful.Matching[L]](tp: Topology[L])(
//      implicit
//      matching: M
//  ) = new EvM[L, M]
//
//  class Dummy2[L <: Law, M <: Lawful.Matching[L]](tp: Topology[L])(
//      implicit
//      matching: M
//  ) extends EvM[L, M]

  import Lawful.LawImpl

  trait AnyGraph extends LawImpl[Arrow]
  object AnyGraph extends Get[AnyGraph] {

    trait Outbound extends AnyGraph with LawImpl[Arrow.`~>`.^]
    object Outbound extends Get[Outbound]
  }

  trait Poset extends AnyGraph
  object Poset extends Get[Poset]

  trait Semilattice extends Poset
  object Semilattice extends Get[Semilattice] {

    trait Upper extends Semilattice with AnyGraph.Outbound
    object Upper extends Get[Upper] {

      implicit class NodeOps[V](n: NodeK.Compat[Upper, V]) {

        def isLeaf: Boolean = n.induction.isEmpty
      }
    }
  }

  trait Tree extends Semilattice.Upper
  object Tree extends Get[Tree]

  { // sanity

    val t1 = Tree
    implicitly[t1._Arrow <:< Arrow]
    implicitly[t1._Arrow <:< Arrow.`~>`.^]
  }

//  { // sanity
//
//    val t1 = dummy1(Tree)
//    implicitly[t1._Arrow <:< Arrow]
//    implicitly[t1._Arrow <:< Arrow.`~>`.^]
//  }
//
//  { // sanity
//
//    val t1 = new Dummy2(Tree)
//    implicitly[t1._Arrow <:< Arrow]
//    implicitly[t1._Arrow <:< Arrow.`~>`.^]
//  }

  //  private def __sanity[V]: Unit = {
  //
  //    implicitly[Poset.StructKS[Int]#Node_~ <:< AnyGraph.StructKS[Int]#Node_~]
  //
  //    implicitly[Poset.StructKS[V]#Node_~ <:< AnyGraph.StructKS[V]#Node_~]
  //
  //    implicitly[Topology[Poset]] // can always summon topology
  //  }
}
