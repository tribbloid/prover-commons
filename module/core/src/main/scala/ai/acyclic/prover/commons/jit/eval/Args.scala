package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.>:>

import ai.acyclic.prover.commons.finset.{Finsets, ToTupleBackbone}
import ai.acyclic.prover.commons.jit.hom
import ai.acyclic.prover.commons.jit.hom.Hom
import zio.Zippable

object Args extends Finsets {

  import Hom.*

  override type VBound = ConstantFn[?]

  trait Fin {}

  trait NoInfo[D] extends Fin {
    // in partial evaluation, none of the arg is provided

    type Peer >: this.type <: Fin

    def self: Peer = this
  }

  protected case object _Empty extends NoInfo[Unit] {

    override type Peer = Empty
  }

  infix trait ><:[+H <: VBound, +T <: Fin] extends Fin {

    def head: H
    def tail: T
  }

  type ><![+X, +T <: Fin] = ConstantFn[X] ><: T

//  case class NoneProvided[D, T <: NoInfo[D], X](tail: T)(
//      implicit
//      zip: Zippable[D, X]
//  ) extends NoInfo[zip.Out]
//      with (X ><! T) {
//
//    override lazy val head: ConstantFn[X] = Const.NotProvided
//
//    override type Peer = ConstantFn[X] ><: tail.Peer
//  }

  override def cons[HEAD <: VBound, TAIL <: Fin](head: HEAD, tail: TAIL): HEAD ><: TAIL = ???

  override def deCons[HEAD <: VBound, TAIL <: Fin](cons: HEAD ><: TAIL): (HEAD, TAIL) =
    cons.head -> cons.tail
}
