package ai.acyclic.prover.commons.tuple

import scala.language.implicitConversions

trait LeftAssociated {

  type VBound

  type Prod

  protected val _1: Prod

  /**
    * Identity element of the product (MATLAB terminology)
    */
  final def Eye: Eye = _1
  type Eye = _1.type

  infix type :><[+TAIL <: Prod, +HEAD <: VBound] <: Prod

  def deCons[TAIL <: Prod, HEAD <: VBound](cons: TAIL :>< HEAD): (TAIL, HEAD)
}
