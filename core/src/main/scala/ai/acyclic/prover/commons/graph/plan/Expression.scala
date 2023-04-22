package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphKind

import scala.language.implicitConversions

trait Expression[+T <: GraphKind.Like] {

//  val t: Topology {type G[V] >: T}

//  type Node[V] = t.Node[V]

  def exe: T

  final lazy val exeOnce: T = exe
}

object Expression {

  case class Leaf[OG <: GraphKind.Like](override val exe: OG) extends Expression[OG]

  implicit def asLeaf[OG <: GraphKind.Like](g: OG): Leaf[OG] = Leaf(g)
}
