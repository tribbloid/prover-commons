package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Topology.GraphT
import ai.acyclic.prover.commons.graph.viz.{Hasse, LinkedHierarchy}

// this is untyped, should always leave the possibility to add dependent type information.
// See project Matryoshka
// OR this article for a possible start:
//  https://macsphere.mcma
//
//  ster.ca/bitstream/11375/18494/2/thesis.pdf
trait Graph[V] extends GraphT._Graph[V] {

  final type Value = V
  final val sys: Local.type = Local

  def isEmpty: Boolean = roots.isEmpty

  final override lazy val rootValues = roots.map(_.value)

  def diagram_Hasse(
      implicit
      format: Hasse
  ): format.Viz[V] = format.Viz(this)
}

object Graph {

  trait Outbound[N] extends Graph[N] with GraphT.OutboundT._Graph[N] {

    def diagram_linkedHierarchy(
        implicit
        group: LinkedHierarchy#Group
    ): group.Viz[N] = group.Viz(this)
  }

  object Outbound {}
}
