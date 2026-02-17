package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.tuple.Products
import ai.acyclic.prover.commons.jit.hom.Hom

object Args extends Products.Monoidal {

  import Hom.*

  override type VBound = ConstantFn[?]

  override type Element[V <: VBound] = V

  /**
    * choose 1 of the 2 options:
    *
    *   - `Fn[(X, Y), T]`, for partial eval, summon [[FromFlatRepr]] then perform on upstreams.
    *   - `Fn[X ><!: Y, T]`, everything starts from partial eval, summon[[FromFlatRepr]] when converting to normal
    *     [[Function1View]]
    *
    * Second option looks more cleaner: only need to summon once as the last step. In the first option, we need to
    * summon repeatedly for recursive partial evaluation/reduction
    */
  trait Prod {

    def schema: Schema
  }

  /**
    * all members are [[Const.NotProvided]], if used in partial eval, will yield an optimised function
    */
  trait Schema extends Prod {

    final override def schema: this.type = this
  }
  object Schema {

    case class ><[+X, +T <: Schema](
        tail: T
    ) extends (ConstantFn[X] ><: T)
        with Schema {

      val head: ConstantFn[X] = Const.NotProvided
    }
  }

  protected case object _1 extends Schema {}

  sealed trait ><:[+H <: ConstantFn[?], +T <: Prod] extends Prod {

    val head: H
    val tail: T
  }

  // Should this defined as a dependent type of Schema (which is a phantom & always available)
  // the only capability it grants is to remove some pending arguments that are guaranteed to be provided
  infix trait ><[+X, +T <: Prod] extends (ConstantFn[X] ><: T) {

    override def schema = Schema.><[X, Schema](tail.schema) // TODO: not narrow enough
  }

  type ><![+X, +Y] = (ConstantFn[X] ><: ConstantFn[Y] ><: Eye) {} // should this be a trait?

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

  override def cons[HEAD <: VBound, TAIL <: Prod](head: Element[HEAD], tail: TAIL): HEAD ><: TAIL = ???

  override def deCons[HEAD <: VBound, TAIL <: Prod](cons: HEAD ><: TAIL): (Element[HEAD], TAIL) = {

    cons.head -> cons.tail
  }
//    cons.head -> cons.tail
}
