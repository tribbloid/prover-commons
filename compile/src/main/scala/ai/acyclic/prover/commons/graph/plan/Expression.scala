package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphKind

import scala.language.implicitConversions

trait Expression[+OG <: GraphKind.Like] {

  def exe: OG

  final lazy val exeOnce: OG = exe

//  type Value = exeOnce.Value
}

object Expression {

  case class Leaf[OG <: GraphKind.Like](override val exe: OG) extends Expression[OG]

  implicit def asLeaf[OG <: GraphKind.Like](g: OG): Leaf[OG] = Leaf(g)
}
