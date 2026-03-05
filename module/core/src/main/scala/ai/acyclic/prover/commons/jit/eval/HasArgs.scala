package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.Hom.Const
import ai.acyclic.prover.commons.tuple.{Products, Schemata}
import ai.acyclic.prover.commons.util.Phantom

import scala.util.Try

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
      * Schema-only phantom
      */
    trait Schema[Peer <: Args] extends Phantom {

      type Top >: Peer <: Args
      type Bottom <: Peer

      type TryComputeAll <: TupleX.Prod
      type ComputeAll <: TupleX.Prod
    }

    object Schema {

      type Gt[-T >: Args] = Schema[? >: T]

      object Eye extends Schema[Args.T0] {

        type TryComputeAll = TupleXEmpty
        type ComputeAll = TupleXEmpty

        override type Top = Args.T0
        override type Bottom = Args.T0
      }
      type Eye = Eye.type

      final infix class SchemaCons[H, T <: Args] private[Schema] () extends Schema[Args.><:[H, T]] {

        lazy val tail: Schema[T] = Phantom.apply()

        type TryComputeAll = Try[H] *: tail.TryComputeAll
        type ComputeAll = H *: tail.ComputeAll

        override type Top = Any ><: T
        override type Bottom = Nothing ><: T
      }
    }

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

      val schema: Schema.Gt[this.type]

      val computeAll: schema.ComputeAll

      val Bottom: schema.Bottom
    }

//    implicit class ProdOps[T <: Prod](val self: T) {
//
//      def consBottom: Nothing ><: self.Bottom = Cons(Const.NotProvided, self.Bottom)
//    }

    override object eye extends Prod with ElementsMixin.Eye {

      lazy val schema: Schema.Eye = Schema.Eye

      override lazy val computeAll = TupleXEmpty

      @transient override lazy val Bottom = this

    }

    type ><:[+H, +T <: Prod] = Cons[? <: H, ? <: T]

    final protected case class Cons[H, T <: Prod] private[Args] (
        head: Element[H],
        tail: T
    ) extends Prod
        with ElementsMixin.><:[H, T] {

      import Schema.SchemaCons

      lazy val schema = Phantom.apply.apply[H SchemaCons T]()

      def computeHead: H = head.compute

      override lazy val computeAll = computeHead *: tail.computeAll

      @transient override lazy val Bottom = cons(Const.NotProvided, tail.Bottom)

      override lazy val runtimeSeq = head +: tail.runtimeSeq

      lazy val valueSeq: Seq[Any] = runtimeSeq.map(_.compute)

    }

    implicitly[Eye =:= T0]
    implicitly[(Int ><: String ><: Eye) =:= (Int >< String)]

    // Should this defined as a dependent type of Schema (which is a phantom & always available)
    // the only capability it grants is to remove some pending arguments that are guaranteed to be provided

    override protected def cons[L, TAIL <: Prod](head: Element[L], tail: TAIL): L ><: TAIL =
      Cons(head, tail)

    override def deCons[L, TAIL <: Prod](cons: L ><: TAIL): (Element[L], TAIL) =
      (cons.head, cons.tail)
  }

//  trait ArgSchema[-I <: Args] {
//
//    type Peer >: I <: Args
//
//    type Bottom <: Peer
//    val Bottom: Bottom
//
//    def getBottom[R >: Bottom <: I]: R
//  }

  trait NoneGenerator[-T <: (Option[String], Option[Int])] {

    val bottom = (None, None)

    def gen[R >: (Option[Nothing], Option[Nothing]) <: T]: R =
      bottom // it is possbile to output value with contravariant bound
  }

}
