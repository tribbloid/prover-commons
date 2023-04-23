package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphKind

import scala.language.implicitConversions

trait Plan[+T <: GraphKind.Top] {

  def compute: T

  final lazy val graph: T = compute
}

object Plan {

  case class Leaf[OG <: GraphKind.Top](override val compute: OG) extends Plan[OG]

  implicit def asLeaf[OG <: GraphKind.Top](g: OG): Leaf[OG] = Leaf(g)
}
