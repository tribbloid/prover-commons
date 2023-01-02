package ai.acyclic.prover.commons.graph

trait GraphSystem {

  type Many[+T] = IndexedSeq[T]
  val Many = IndexedSeq

//  type Node[_, _] <: _Node
//  type Node[+PEER <: Graph.Node[PEER, AA <: Arrow.Of[NodeLike]], +AA <: Arrow.Of[NodeLike]] <: NodeLike
//  def Node() // TODO: enable minimal constructor

  type OpsOf[+N <: Node] <: Graph.OpsOf[Node]

  type InductiveMixin = OpsOf[Node]

  type Node <: Graph.Node with InductiveMixin

  type NodeOf[+N <: Node] = Node with OpsOf[N]

  type ArrowUB = Arrow.Of[Node]
}

object GraphSystem {}
