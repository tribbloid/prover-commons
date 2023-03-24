package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.{HasOuter, Same}
import ai.acyclic.prover.commons.graph.Connection.Topology

trait GraphK[N] extends GraphK.Like {

  final type NodeType = N

  def roots: Rows[N]

  type Ops <: Connection[N]
  protected def Ops: Topology[N, Ops]

  lazy val nodeOps: Topology[N, Ops] = Ops
}

object GraphK {

  trait Like extends HasOuter {

    type NodeType

    val sys: GraphSystem

    final def outer: GraphSystem = sys

    lazy val sameness: Same.Definition = Same.ByEquality

    type Rows[T] = sys.Dataset[T]
  }

  trait Immutable[N] extends GraphK[N] {

    // the Memoization appears to confine GraphK to be only applicable to immutable graph
    //  can this be lifted?
    override lazy val nodeOps: Topology[N, Ops] =
      sameness
        .Memoize[N, Ops](
          Ops
        )
  }
}
