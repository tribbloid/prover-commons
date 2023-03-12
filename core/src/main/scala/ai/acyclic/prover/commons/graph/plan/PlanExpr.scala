package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphSystem

import scala.language.implicitConversions

trait PlanExpr[OG <: GraphSystem._Graph] {

  def exe: OG

  final lazy val exeOnce: OG = exe

  type NodeType = exeOnce.NodeType
}

object PlanExpr {

  case class Leaf[OG <: GraphSystem._Graph](override val exe: OG) extends PlanExpr[OG]

  implicit def asLeaf[OG <: GraphSystem._Graph](g: OG): Leaf[OG] = Leaf(g)
}
