package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.Topology

import scala.language.implicitConversions

trait Plan {

  type OT <: Topology
  val outTopology: OT

  type OV

  final type OGK[v] = outTopology.G[v]
  final type OG = OGK[OV]
  final type ON = outTopology.Node[OV]

  def compute: OG

  final lazy val resolve: OG = compute

}

object Plan {

//  type Aux[TT <: Topology] = Plan { type OT = TT }
}
