package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.Structure

import scala.language.implicitConversions

trait Expression[+OG <: Structure.Like] {

  def exe: OG

  final lazy val exeOnce: OG = exe

  type Value = exeOnce.Value
}

object Expression {

  case class Leaf[OG <: Structure.Like](override val exe: OG) extends Expression[OG]

  implicit def asLeaf[OG <: Structure.Like](g: OG): Leaf[OG] = Leaf(g)
}
