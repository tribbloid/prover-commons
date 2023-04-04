package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.{HasOuter, Same}

trait GraphK[+Ops <: Induction.Like[_, _]] extends GraphK.Like {

  type Node

  def roots: Dataset[Node]
//  lazy val rootOps: Dataset[Ops] = roots.map(v => ops(v))

  protected def Ops: Node => Ops

  lazy val ops: Node => Ops = Ops
}

object GraphK {

  trait Like extends HasOuter {

    type Node

    val sys: GraphSystem

    final def outer: GraphSystem = sys

    lazy val sameness: Same.Definition = Same.ByEquality

    type Dataset[T] = sys.Dataset[T]
  }

  trait Immutable[+Ops <: Induction.Like[_, _]] extends GraphK[Ops] {

    // the Memoization appears to confine GraphK to be only applicable to immutable graph
    //  can this be lifted?
    override lazy val ops: Node => Ops =
      sameness
        .Memoize[Node, Ops](
          Ops
        )
  }
}
