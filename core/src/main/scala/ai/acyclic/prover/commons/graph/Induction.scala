package ai.acyclic.prover.commons.graph

import scala.language.implicitConversions

trait Induction[N, +A <: Arrow, +SELF <: Induction[N, A, SELF]] extends Induction.Like[A, SELF] {

  import Induction._

  final override type Node = N

  protected def getInduction: Seq[(A, SELF)]
  lazy val induction = getInduction

  final lazy val discoverInduction: Seq[SELF] = getInduction.map(_._2)

  final lazy val discoverArrows: Many[(A, N)] = induction.map(v => v._1 -> v._2.node)

  final lazy val discoverNodes: Many[N] = discoverArrows.map(_._2)
}

object Induction {

  trait Like[+A <: Arrow, +SELF <: Like[A, SELF]] {

    type Node
    val node: Node

    protected def getNodeText: String = node.toString

    final lazy val nodeText: String = getNodeText
  }

  trait Cap extends Like[Arrow, Cap] {}

  type Many[+A] = Vector[A]

  implicit def toMany[A](value: Seq[A]): Many[A] = value.toVector

  implicit def unbox(i: Cap): i.Node = i.node

}
