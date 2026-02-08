package ai.acyclic.prover.commons.finset

import scala.language.implicitConversions

/**
  * right nested tuple system
  *   - head to the left
  *   - tail to the right
  */
trait RightNested {

  type VBound

  type Fin

  protected val _Empty: Fin
  type Empty = _Empty.type
  final def Empty: Empty = _Empty

  infix type ><:[+HEAD <: VBound, +TAIL <: Fin] <: Fin // <- bra-ket notation for product

  def cons[HEAD <: VBound, TAIL <: Fin](head: HEAD, tail: TAIL): HEAD ><: TAIL

  def deCons[HEAD <: VBound, TAIL <: Fin](cons: HEAD ><: TAIL): (HEAD, TAIL)

  sealed trait _TupleOps[SELF <: Fin] {

    def self: SELF

    def ><:[
        HEAD <: VBound
    ](
        head: HEAD
    ): HEAD ><: SELF = cons(head, self)
  }

  implicit class tupleOps[SELF <: Fin](val self: SELF) extends _TupleOps[SELF] {}

  implicit def eyeExtension(s: this.type): tupleOps[Empty] = tupleOps[Empty](Empty)
}
