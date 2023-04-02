package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphK

import scala.language.implicitConversions

trait PlanExpr[+OG <: GraphK.Like] {

  def exe: OG

  final lazy val exeOnce: OG = exe

  type NodeType = exeOnce.Node
}

object PlanExpr {

  case class Leaf[OG <: GraphK.Like](override val exe: OG) extends PlanExpr[OG]

  implicit def asLeaf[OG <: GraphK.Like](g: OG): Leaf[OG] = Leaf(g)
}
