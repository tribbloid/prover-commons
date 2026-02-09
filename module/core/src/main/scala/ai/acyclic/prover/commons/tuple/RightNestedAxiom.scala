package ai.acyclic.prover.commons.tuple

import scala.language.implicitConversions

/**
  * right nested tuple system
  *   - head to the left
  *   - tail to the right
  */
trait RightNestedAxiom {

  type VBound

  type Inductive

  // aliases
  type Empty <: Inductive
  protected val _0: Empty
  final def Empty: Empty = _0

  type Nil = Empty
  val Nil: Nil = Empty

  infix type ><:[+HEAD <: VBound, +TAIL <: Inductive] <: Inductive // <- bra-ket notation for product

  def deCons[HEAD <: VBound, TAIL <: Inductive](cons: HEAD ><: TAIL): (HEAD, TAIL)

}
