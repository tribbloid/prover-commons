package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphK

import scala.language.implicitConversions

trait Expression[+OG <: GraphK.Like] {

  def exe: OG

  final lazy val exeOnce: OG = exe

  type Node = exeOnce.Node
}

object Expression {

  case class Leaf[OG <: GraphK.Like](override val exe: OG) extends Expression[OG]

  implicit def asLeaf[OG <: GraphK.Like](g: OG): Leaf[OG] = Leaf(g)
}
