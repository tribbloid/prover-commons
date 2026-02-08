package ai.acyclic.prover.commons.tuple

import scala.language.implicitConversions

/**
  * left nested tuple system
  *   - tail to the left
  *   - head to the right
  *
  * TODO: most other libraries (shapeless :: or Scala 3 *:) are right-nested, should I comply?
  */
trait LeftNestedAxiom {

  type VBound

  type Inductive

  protected val _Empty: Inductive
  type Empty = _Empty.type
  final def Empty: Empty = _Empty

  infix type ><[+TAIL <: Inductive, +HEAD <: VBound] <: Inductive // <- ket-bra notation for product

  infix type :><[+TAIL <: Inductive, +HEAD <: VBound] = TAIL >< HEAD // <- ket-bra notation for product

  def deCons[TAIL <: Inductive, HEAD <: VBound](cons: TAIL >< HEAD): (TAIL, HEAD)

}
