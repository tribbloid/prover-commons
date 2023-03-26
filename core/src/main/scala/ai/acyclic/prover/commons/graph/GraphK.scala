package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.{HasOuter, Same}
import ai.acyclic.prover.commons.graph.TopologyOps.Topology

trait GraphK[N] extends GraphK.Like {

  final type NodeType = N

  def roots: Dataset[N]

  type Ops <: TopologyOps[N]
  protected def Ops: Topology[N, Ops]

  lazy val ops: Topology[N, Ops] = Ops
}

object GraphK {

  trait Like extends HasOuter {

    type NodeType

    val sys: GraphSystem

    final def outer: GraphSystem = sys

    lazy val sameness: Same.Definition = Same.ByEquality

    type Dataset[T] = sys.Dataset[T]
  }

  trait Immutable[N] extends GraphK[N] {

    // the Memoization appears to confine GraphK to be only applicable to immutable graph
    //  can this be lifted?
    override lazy val ops: Topology[N, Ops] =
      sameness
        .Memoize[N, Ops](
          Ops
        )
  }
}
