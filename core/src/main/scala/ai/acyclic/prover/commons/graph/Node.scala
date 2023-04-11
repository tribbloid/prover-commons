package ai.acyclic.prover.commons.graph

import scala.language.implicitConversions

trait Node[+V, +A <: Arrow, +SELF <: Node[V, A, SELF]] extends Node.Like[A, SELF] {

  import Node._

  override val value: V

  protected def getInduction: Seq[(A, SELF)]
  lazy val induction = getInduction

  final lazy val discoverNodes: Seq[SELF] = induction.map(_._2)

  final lazy val valueInduction: Many[(A, V)] = induction.map(v => v._1 -> v._2.value)

  final lazy val discoverValues: Many[V] = valueInduction.map(_._2)
}

object Node {

  trait Like[+A <: Arrow, +SELF <: Like[A, SELF]] {

    val value: Any

    protected def getNodeText: String = value.toString

    final lazy val nodeText: String = getNodeText
  }

//  type Cap[N] = Induction[N, Arrow, Induction[N, Arrow, _]] {}

  type Many[+A] = Vector[A]

  implicit def toMany[A](value: Seq[A]): Many[A] = value.toVector

  implicit def unbox[N](i: Node[N, Arrow, _]): N = i.value

}
