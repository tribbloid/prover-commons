package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.viz.Hierarchy

trait Semilattice[N] extends Poset[N] {

  def root: N

  final lazy val roots: Rows[N] = sys.parallelize(Seq(root))
}

object Semilattice {

  trait Upper[N] extends Semilattice[N] with Poset.Upper[N] {

    trait UpperNOps extends OutboundNOps {

      lazy val allOffsprings: Seq[N] = {
        val cc = children
        val others = children.flatMap { child =>
          val ops = nodeOps(child)
          ops.allOffsprings
        }
        cc ++ others
      }
    }

    type Ops <: UpperNOps

    def diagram_hierarchy(
        implicit
        format: Hierarchy
    ): format.Viz[N] = format.Viz(this)
  }

  object Upper {}
}
