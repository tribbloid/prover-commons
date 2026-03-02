package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.Hom.Const
import ai.acyclic.prover.commons.tuple.{Products, Schemata}

trait HasArgs {

  import Args.><:

  type Args = Args.Prod
  type Arg0 = Args.Eye
  type Arg1[X] = X ><: Args.Eye
  type Args2[X, Y] = X ><: Y ><: Args.Eye

  object Args extends Products.Monoidal with Schemata.Cartesian_UID {

    import TupleX.*

    override type VBound = Any

    override type Element[V] = Hom.ConstantFn[V]

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

      type Peer <: Prod
      type Top >: Peer <: Prod
      type Bottom <: Peer
      val Bottom: Bottom

      type ComputeAll <: TupleX.Prod
      val computeAll: ComputeAll
    }

    override object eye extends Prod with ElementsMixin.Eye {

      type ComputeAll = TupleXEmpty
      override lazy val computeAll: ComputeAll = TupleXEmpty

      override type Peer = this.type
      override type Top = this.type
      override type Bottom = this.type
      override val Bottom = this
    }

    type ><:[+H, +T <: Prod] = Cons[? <: H, T]

    case class Cons[H, +T <: Prod] private[Args] (
        head: Element[H],
        tail: T
    ) extends Prod
        with ElementsMixin.><:[H, T] {

      type ComputeAll = H *: tail.ComputeAll
      override lazy val computeAll: ComputeAll = computeHead *: tail.computeAll

      def computeHead: H = head.compute

      override lazy val runtimeSeq = head +: tail.runtimeSeq

      lazy val valueSeq: Seq[Any] = runtimeSeq.map(_.compute)

      override type Peer = H ><: tail.Peer
      override type Top = Any ><: tail.Top
      override type Bottom = Nothing ><: tail.Bottom
      override lazy val Bottom: Bottom = {

        val head: Element[Nothing] = Const.NotProvided
        head ><: tail.Bottom
      }
    }

    def consNotProvided[H, T](tail: T) = Cons(Const.NotProvided, tail)

    implicitly[Eye =:= T0]
    implicitly[(Int ><: String ><: Eye) =:= (Int >< String)]

    // Should this defined as a dependent type of Schema (which is a phantom & always available)
    // the only capability it grants is to remove some pending arguments that are guaranteed to be provided

    override protected def cons[L, TAIL <: Prod](head: Element[L], tail: TAIL): L ><: TAIL =
      Cons(head, tail)

    override def deCons[L, TAIL <: Prod](cons: L ><: TAIL): (Element[L], TAIL) =
      (cons.head, cons.tail)

  }

}
