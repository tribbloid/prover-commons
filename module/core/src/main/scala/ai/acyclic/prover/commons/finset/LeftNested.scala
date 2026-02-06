package ai.acyclic.prover.commons.finset

import scala.language.implicitConversions

/**
  * left nested tuple system
  *   - tail to the left
  *   - head to the right
  *
  * TODO: most other libraries (shapeless :: or Scala 3 *:) are right-nested, should I comply?
  */
trait LeftNested {

  type VBound

  type Fin

  type Eye <: Fin
  val Eye: Eye

  infix type ><[+TAIL <: Fin, +HEAD <: VBound] <: Fin // <- ket-bra notation for product

  infix type :><[+TAIL <: Fin, +HEAD <: VBound] = TAIL >< HEAD // <- ket-bra notation for product

  def cons[TAIL <: Fin, HEAD <: VBound](tail: TAIL, head: HEAD): TAIL >< HEAD

  def deCons[TAIL <: Fin, HEAD <: VBound](cons: TAIL >< HEAD): (TAIL, HEAD)

  sealed trait _TupleOps[SELF <: Fin] {

    def self: SELF

    def ><[
        HEAD <: VBound
    ](
        head: HEAD
    ): SELF >< HEAD = cons(self, head)

//    def &[
//        HEAD <: VBound
//    ](
//        head: HEAD
//    ): SELF >< HEAD = cons(self, head) // TODO: remove
  }

  implicit class tupleOps[SELF <: Fin](val self: SELF) extends _TupleOps[SELF] {}

  implicit def eyeExtension(s: this.type): tupleOps[Eye] = tupleOps[Eye](Eye)
}
