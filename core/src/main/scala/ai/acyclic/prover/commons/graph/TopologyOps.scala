package ai.acyclic.prover.commons.graph

import scala.language.implicitConversions

trait TopologyOps[+N] extends TopologyOps.NodeMixin[N] with TopologyOps.InductionMixin[N, Arrow.Of[N]] {}

object TopologyOps {

  type Many[+A] = Vector[A]

  implicit def toMany[A](value: Seq[A]): Many[A] = value.toVector

  implicit def unbox[N](ops: TopologyOps[N]): N = ops.node

  trait InductionMixin[+N, +A <: Arrow.Of[N]] {

    protected def getInduction: Seq[A]

    final lazy val induction: Many[A] = getInduction

    final lazy val canDiscover: Many[N] = induction.map(_.target)
  }

  trait NodeMixin[+N] {

    val node: N

    protected def getNodeText: String = node.toString

    final lazy val nodeText: String = getNodeText
  }

  type Topology[-I, +O <: TopologyOps[_]] = I => O
}

//trait Topology[N, NR <: N] extends (NR => Seq[N]) {}
