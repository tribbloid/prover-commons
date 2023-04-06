package ai.acyclic.prover.commons.graph

import scala.language.implicitConversions

trait Induction[+V, +A <: Arrow, +SELF <: Induction[V, A, SELF]] extends Induction.Like[A, SELF] {

  import Induction._

  override val value: V

  protected def getInduction: Seq[(A, SELF)]
  lazy val induction = getInduction

  final lazy val discoverInduction: Seq[SELF] = getInduction.map(_._2)

  final lazy val discoverArrows: Many[(A, V)] = induction.map(v => v._1 -> v._2.value)

  final lazy val discoverNodes: Many[V] = discoverArrows.map(_._2)
}

object Induction {

  trait Like[+A <: Arrow, +SELF <: Like[A, SELF]] {

    val value: Any

    protected def getNodeText: String = value.toString

    final lazy val nodeText: String = getNodeText
  }

//  type Cap[N] = Induction[N, Arrow, Induction[N, Arrow, _]] {}

  type Many[+A] = Vector[A]

  implicit def toMany[A](value: Seq[A]): Many[A] = value.toVector

  implicit def unbox[N](i: Induction[N, Arrow, _]): N = i.value

}
