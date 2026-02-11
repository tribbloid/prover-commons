package ai.acyclic.prover.commons.tuple

import scala.language.implicitConversions

/**
  * right nested tuple system
  *   - head to the left
  *   - tail to the right
  */
trait RightAssociated {

  type VBound

  type Prod

  protected val _1: Prod

  /**
    * Identity element of the product (MATLAB terminology)
    */
  final def Eye: Eye = _1
  type Eye = _1.type

  type Nil = Eye
  val Nil: Nil = Eye

  /**
    * The product (Bra-ket notation)
    */
  infix type ><:[+HEAD <: VBound, +TAIL <: Prod] <: Prod

  def deCons[HEAD <: VBound, TAIL <: Prod](cons: HEAD ><: TAIL): (HEAD, TAIL)

}
