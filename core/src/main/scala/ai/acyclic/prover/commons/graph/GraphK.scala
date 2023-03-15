package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.{HasOuter, Same}

trait GraphK[N] extends GraphSystem._Graph {

  final type NodeType = N

  trait NodeOps extends InductionBy[Arrow.Of[N]] with HasOuter {

    def outer: GraphK[N] = GraphK.this
  }

  def roots: Rows[N]

  type Ops <: NodeOps
  protected val Ops: N => Ops

  lazy val samenessEv: Same.Definition = Same.ByEquality

  def _nodeOps: N => Ops = { v =>
    Ops(v)
  }
  final lazy val nodeOps: N => Ops = _nodeOps

  trait InductionBy[+A <: Arrow.Of[N]] {

    protected def getInduction: Seq[A]

    final lazy val induction: Many[A] = sys.toMany(getInduction)

    final lazy val canDiscover: Many[N] = sys.toMany(induction.map(_.target))
  }

}

object GraphK {

  trait Immutable[N] extends GraphK[N] {

    // the Memoization appears to confine GraphK to be only applicable to immutable graph
    //  can this be lifted?
    override def _nodeOps: N => Ops =
      samenessEv
        .Memoize[N, Ops](
          super._nodeOps
        )
  }
}
