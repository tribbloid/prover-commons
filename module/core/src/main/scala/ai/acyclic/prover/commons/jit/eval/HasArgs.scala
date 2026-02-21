package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.tuple.Products
import ai.acyclic.prover.commons.jit.hom.Hom

trait HasArgs {

  type Args = Args.Prod

  object Args extends Products.Monoidal with Products.Cartesian_UID {

    import Hom.*
    import TupleX.*

    override type VBound = Any

    override type Element[V] = ConstantFn[V]

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
    sealed trait Prod extends ElementsMixin.Prod {

      type ComputeAll <: TupleX.Prod
      val computeAll: ComputeAll
    }

    override object eye extends Prod with ElementsMixin.Eye {

      type ComputeAll = TupleXEmpty
      override lazy val computeAll: ComputeAll = TupleXEmpty

    }

    case class ><:[H, T <: Prod](
        head: ConstantFn[H],
        tail: T
    ) extends Prod
        with ElementsMixin.><:[H, T] {

      type ComputeAll = H *: tail.ComputeAll
      override lazy val computeAll: ComputeAll = computeHead *: tail.computeAll

      def computeHead: H = head.compute

      override lazy val runtimeSeq = head +: tail.runtimeSeq

      lazy val valueSeq: Seq[Any] = runtimeSeq.map(_.compute)
    }

    implicitly[(Int ><: String ><: Eye) =:= (Int >< String)]

    // Should this defined as a dependent type of Schema (which is a phantom & always available)
    // the only capability it grants is to remove some pending arguments that are guaranteed to be provided

    override def cons[L, TAIL <: Prod](head: Hom.ConstantFn[L], tail: TAIL): L ><: TAIL =
      ><:(head, tail)

    override def deCons[L, TAIL <: Prod](cons: L ><: TAIL): (Hom.ConstantFn[L], TAIL) =
      (cons.head, cons.tail)

  }

}
