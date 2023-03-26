package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.viz.Hierarchy

trait Semilattice[N] extends Poset[N] {

  def root: N

  final lazy val roots: Dataset[N] = sys.parallelize(Seq(root))
}

object Semilattice {

  trait Upper[N] extends Semilattice[N] with Poset.Upper[N] {

    trait UpperOps extends OutboundOps {

      lazy val allOffsprings: Seq[N] = {
        val cc = children
        val others = children.flatMap { child =>
          ops(child).allOffsprings
        }
        cc ++ others
      }
    }

    type Ops <: UpperOps

    def diagram_hierarchy(
        implicit
        format: Hierarchy
    ): format.Viz[N] = format.Viz(this)
  }

  object Upper {}
}
