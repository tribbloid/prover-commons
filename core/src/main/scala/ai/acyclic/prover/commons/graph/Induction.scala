package ai.acyclic.prover.commons.graph

import scala.language.implicitConversions

trait Induction extends Induction.Mixin[Arrow, Induction] {}

object Induction {

  type Many[+A] = Vector[A]

  implicit def toMany[A](value: Seq[A]): Many[A] = value.toVector

  implicit def unbox(i: Induction): i.Node = i.node

  trait Mixin[+A <: Arrow, +SELF <: Mixin[A, SELF]] {

    type Node

    val node: Node

    protected def getNodeText: String = node.toString

    final lazy val nodeText: String = getNodeText

    protected def getInduction: Seq[(A, Node)]

    final lazy val induction: Many[(A, Node)] = getInduction

    final lazy val canDiscover: Many[Node] = induction.map(_._2)
  }
}
