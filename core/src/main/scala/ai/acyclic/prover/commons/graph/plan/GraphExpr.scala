package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphSystem

import scala.language.implicitConversions

trait GraphExpr[OG <: GraphSystem._Graph] {

  protected def exe: OG

  final lazy val resolve: OG = exe

  type NodeType = resolve.NodeType
}

object GraphExpr {

  case class Leaf[OG <: GraphSystem._Graph](override val exe: OG) extends GraphExpr[OG]

  implicit def asLeaf[OG <: GraphSystem._Graph](g: OG): Leaf[OG] = Leaf(g)
}
