package ai.acyclic.prover.commons.tuple

trait LeftAssociated extends Associated {

  infix type :><[+TAIL <: Prod, +HEAD <: VBound] <: Prod

  def deCons[TAIL <: Prod, HEAD <: VBound](cons: TAIL :>< HEAD): (TAIL, HEAD)
}
