package ai.acyclic.prover.commons.graph

import scala.language.implicitConversions

trait Induction[N, +A <: Arrow, +SELF <: Induction[N, A, SELF]] extends Induction.Like[A, SELF] {

  import Induction._

  final override type Node = N

  protected def getInduction: Seq[(A, N)] // = arrows.map(v => v._1 -> v._2.node)

  // TODO: migrate to:
  //    protected def getArrows: Seq[(A, SELF)]
  //    lazy val arrows = getArrows
  //
  //    protected lazy val getInduction: Seq[(A, Node)] = arrows.map(v => v._1 -> v._2.node)

  final lazy val induction: Many[(A, N)] = getInduction

  final lazy val canDiscover: Many[N] = induction.map(_._2)
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
