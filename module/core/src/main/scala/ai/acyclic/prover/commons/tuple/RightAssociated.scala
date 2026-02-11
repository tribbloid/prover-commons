package ai.acyclic.prover.commons.tuple

/**
  * right nested tuple system
  *   - head to the left
  *   - tail to the right
  */
trait RightAssociated extends Associated {

  /**
    * The product (Bra-ket notation)
    */
  infix type ><:[+HEAD <: VBound, +TAIL <: Prod] <: Prod

  def deCons[HEAD <: VBound, TAIL <: Prod](cons: HEAD ><: TAIL): (HEAD, TAIL)

}
