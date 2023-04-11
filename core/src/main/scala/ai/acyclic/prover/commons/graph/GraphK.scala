package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.{HasOuter, Same}

trait GraphK[+Ops <: Node.Like[_, _]] extends GraphK.Like {

  type Value

  def roots: Dataset[Ops]
  def rootValues: Dataset[Value]

//  protected def Ops: Value => Ops
//
//  lazy val ops: Value => Ops = Ops
}

object GraphK {

  trait Like extends HasOuter {

    type Value

    val sys: GraphSystem

    final def outer: GraphSystem = sys

    // should only compare the sameness of node, NOT value!
    //  otherwise a mapping to the value may introduce forbidden subgraph(s).
    lazy val sameness: Same.Definition = Same.ByEquality

    type Dataset[T] = sys.Dataset[T]
  }

  // TODO: don't know how to define it in the new architecture`
  trait Immutable[+Ops <: Node.Like[_, _]] extends GraphK[Ops] {

    // the Memoization appears to confine GraphK to be only applicable to immutable graph
    //  can this be lifted?
    override lazy val ops: Value => Ops =
      sameness
        .Memoize[Value, Ops](
          Ops
        )
  }
}
