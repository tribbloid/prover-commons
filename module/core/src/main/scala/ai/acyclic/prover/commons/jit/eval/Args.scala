package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.>:>

import ai.acyclic.prover.commons.tuple.BTuples
import ai.acyclic.prover.commons.jit.hom
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.tuple.backbone.RecursiveHeapBackbone
import zio.Zippable

object Args extends BTuples {

  import Hom.*

  override type VBound = ConstantFn[?]

  trait Inductive {}

  trait NoInfo[D] extends Inductive {
    // in partial evaluation, none of the arg is provided

    type Peer >: this.type <: Inductive

    def self: Peer = this
  }

  protected case object _0 extends NoInfo[Unit] {

    override type Peer = Empty
  }

  infix trait ><:[+H <: VBound, +T <: Inductive] extends Inductive {

    def head: H
    def tail: T
  }

  type ><![+X, +T <: Inductive] = ConstantFn[X] ><: T

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

  override def cons[HEAD <: VBound, TAIL <: Inductive](head: HEAD, tail: TAIL): HEAD ><: TAIL = ???

  override def deCons[HEAD <: VBound, TAIL <: Inductive](cons: HEAD ><: TAIL): (HEAD, TAIL) =
    cons.head -> cons.tail
}
