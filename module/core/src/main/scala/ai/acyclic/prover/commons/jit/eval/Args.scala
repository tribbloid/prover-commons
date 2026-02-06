package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.>:>
import ai.acyclic.prover.commons.finset.ToTupleBackbone.><
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

  infix trait ><[+T <: Fin, +H <: VBound] extends Fin {

    def tail: T
    def head: H
  }

  type ><![+T <: Fin, +X] = T >< ConstantFn[X]

//  case class NoneProvided[D, T <: NoInfo[D], X](tail: T)(
//      implicit
//      zip: Zippable[D, X]
//  ) extends NoInfo[zip.Out]
//      with (T ><! X) {
//
//    override lazy val head: ConstantFn[X] = Const.NotProvided
//
//    override type Peer = tail.Peer >< ConstantFn[X]
//  }

  override def cons[TAIL <: Fin, HEAD <: VBound](tail: TAIL, head: HEAD): TAIL >< HEAD = ???

  override def deCons[TAIL <: Fin, HEAD <: VBound](cons: TAIL >< HEAD): (TAIL, HEAD) =
    cons.tail -> cons.head
}
