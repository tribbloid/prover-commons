package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same

trait Platform {
  self: Singleton =>

  import Topology._

  type Dataset[T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait ThisStructure {
    self: Structure[_, _, _] =>

    final abstract override val platform = Platform.this
  }

  case class _Structure[+C <: Constraint, +A <: Arrow, V](
      override val roots: Dataset[Node[C, A, V]],
      override val sameness: Same.Definition = Same.ByEquality,
      nodeText: V => String = (v: V) => v.toString
  ) extends Structure[C, A, V]
      with ThisStructure {}

  def buildFromNodes[C <: Constraint, A <: Arrow, V](
      nodes: Node[C, A, V]*
  ): _Structure[C, A, V] =
    _Structure(parallelize(nodes))

//  type _Struct[T <: Topology, V] = _Structure[T#CC, T#_Arrow, V]

  trait Aliases {

    type Graph[V] = GraphT.SS[V] with ThisStructure

    object Graph {
      type Outbound[V] = GraphT.OutboundT.SS[V] with ThisStructure
    }

    type Poset[V] = PosetT.SS[V] with ThisStructure

    type Semilattice[V] = SemilatticeT.SS[V] with ThisStructure

    object Semilattice {
      type Upper[V] = SemilatticeT.UpperT.SS[V] with ThisStructure
    }

    type Tree[V] = TreeT.SS[V] with ThisStructure
  }
}

object Platform {}
