package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.tuple.{Products, Schemata}
import ai.acyclic.prover.commons.jit.hom.Hom

object Args extends Products.Monoidal with Products.Cartesian_UID {

  import Hom.*

  override type VBound = Any

  override type Element[V <: VBound] = ConstantFn[V]

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
  sealed trait Prod extends ElementsMixin.Prod {}

  object eye extends Prod with ElementsMixin.Eye

  case class ><:[H, T <: Prod](
      element: ConstantFn[H],
      tail: T
  ) extends Prod
      with ElementsMixin.><:[H, T] {

    override def runtimeSeq = element +: tail.runtimeSeq
  }

  // Should this defined as a dependent type of Schema (which is a phantom & always available)
  // the only capability it grants is to remove some pending arguments that are guaranteed to be provided

  override def cons[L <: Args.VBound, TAIL <: Prod](head: Hom.ConstantFn[L], tail: TAIL): L ><: TAIL =
    new ><:(head, tail)

  override def deCons[L <: Args.VBound, TAIL <: Prod](cons: L ><: TAIL): (Hom.ConstantFn[L], TAIL) =
    (cons.element, cons.tail)

}
