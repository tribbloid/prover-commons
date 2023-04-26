package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same

import scala.language.implicitConversions

trait GraphKind[+L <: Topology.Law, +A <: Arrow] {

  val engine: Engine

  // should only compare the sameness of node, NOT value!
  //  otherwise a mapping to the value may introduce forbidden subgraph(s).
  def nodeSameness: Same.Definition = Same.ByEquality

  type Value

//  final type Peer = GraphKind[C, A, V]
//  final type Node = NodeKind.Aux[C, A, V]
//  type LesserNode <: NodeKind.Lt[C, A, V]

  def entriesC: engine.Dataset[NodeKind.Lt[L, A, Value]]
}

object GraphKind {

  type Top = GraphKind[_, _]

  type Aux[+L <: Topology.Law, +A <: Arrow, V] = GraphKind[L, A] { type Value = V }

  trait AuxT[+L <: Topology.Law, +A <: Arrow, V] extends GraphKind[L, A] {
    type Value = V
  }

  // Acronym of "Less Than"
  type Lt[+C <: Topology.Law, +A <: Arrow, +V] = Aux[C, A, _ <: V]

  // TODO: don't know how to define it in the new architecture`
//  trait Immutable[+Ops <: Node.Like[_, _]] extends GraphK[Ops] {
//
//    // the Memoization appears to confine GraphK to be only applicable to immutable graph
//    //  can this be lifted?
//    override lazy val ops: Value => Ops =
//      sameness
//        .Memoize[Value, Ops](
//          Ops
//        )
//  }

}
