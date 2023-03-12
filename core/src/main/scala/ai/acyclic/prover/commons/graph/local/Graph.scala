package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.viz.{Hasse, LinkedHierarchy}
import ai.acyclic.prover.commons.graph.{Arrow, GraphSystem}

// this is untyped, should always leave the possibility to add dependent type information.
// See project Matryoshka
// OR this article for a possible start:
//  https://macsphere.mcmaster.ca/bitstream/11375/18494/2/thesis.pdf
trait Graph[N] extends GraphSystem.GraphK[N] {

  final val sys: Local.type = Local

  lazy val rootOps: Rows[Ops] = roots.map(v => nodeOps(v))

  def isEmpty: Boolean = roots.isEmpty

  trait GraphNOps extends GraphSystem.HasNode[N] with NodeOps {

    final lazy val directEdges = induction.collect {
      case v if v.arrowType.isInstanceOf[Arrow.Edge] => v
    }

    def resolve(): Unit = { induction; nodeText }
  }
  type Ops <: GraphNOps

  def diagram_Hasse(
      implicit
      format: Hasse
  ): format.Viz[N] = format.Viz(this)
}

object Graph {

  trait Outbound[N] extends Graph[N] {

    trait OutboundNOps extends GraphNOps with InductionBy[Arrow.`~>`.Of[N]] {

      protected def getInduction: Seq[Arrow.`~>`.Of[N]] = Nil

      final lazy val children: Seq[N] = {
        induction.map(v => v.target)
      }

      lazy val isLeaf: Boolean = children.isEmpty
    }

    type Ops <: OutboundNOps

    def diagram_linkedHierarchy(
        implicit
        group: LinkedHierarchy#Group
    ): group.Viz[N] = group.Viz(this)
  }

  object Outbound {}

}
