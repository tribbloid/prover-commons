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
    trait Schema[Peer <: Args] extends Serializable {

      type Top >: Peer <: Args
      type Bottom <: Peer

      type TryComputeAll <: TupleX.Prod
      type ComputeAll <: TupleX.Prod

      def bottom: Bottom
    }

    object Schema {

      type Gt[T <: Args] = Schema[? >: T <: Args]

      object Eye extends Schema[Args.T0] {

        type TryComputeAll = TupleXEmpty
        type ComputeAll = TupleXEmpty

        override type Top = Args.T0
        override type Bottom = Args.T0

        override def bottom: Bottom = Args.T0
      }
      type Eye = Eye.type

      final infix class SchemaCons[H, T <: Args] private[Schema] (
          getTail: () => Schema[T]
      ) extends Schema[Args.><:[H, T]] {

        lazy val tail: Schema[T] = getTail()

        type TryComputeAll = Try[H] *: tail.TryComputeAll
        type ComputeAll = H *: tail.ComputeAll

        override type Top = Any ><: T
        override type Bottom = Nothing ><: T

        override def bottom: Bottom = Const.NotProvided ><: tail.bottom
      }

      final def cons[H, T <: Args](tailSchema: Schema[T]): Schema[H ><: T] = {
        new SchemaCons[H, T](() => tailSchema)
      }

      implicit lazy val _eye: Schema[Args.T0] = Eye

      implicit def _cons[H, T <: Args](
          implicit
          tailSchema: Schema[T]
      ): Schema[H ><: T] = {
        cons[H, T](tailSchema)
      }

      object WildCard extends Schema[Args] {

        type TryComputeAll = TupleX
        type ComputeAll = TupleX

        override type Top = Args
        override type Bottom = Nothing

        override def bottom: Bottom = throw new IllegalAccessError("WildCard doesn't have a bottom element")
      }

      implicit lazy val _wildCard: Schema[Args.Prod] = WildCard

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
    }

//    implicit class ProdOps[T <: Prod](val self: T) {
//
//      def consBottom: Nothing ><: self.Bottom = Cons(Const.NotProvided, self.Bottom)
//    }

    override object eye extends Prod with ElementsMixin.Eye {

      lazy val schema: Schema.Eye = Schema.Eye

      override lazy val computeAll: schema.ComputeAll = TupleXEmpty

    }

    type ><:[+H, +T <: Prod] = Cons[? <: H, ? <: T]

    final protected case class Cons[H, T <: Prod] private[Args] (
        head: Element[H],
        tail: T
    ) extends Prod
        with ElementsMixin.><:[H, T] {

      lazy val schema: Schema[Args.><:[H, T]] { type ComputeAll = H *: tail.schema.ComputeAll } = ???

      override lazy val computeAll: schema.ComputeAll = head.compute *: tail.computeAll

      override lazy val runtimeSeq = head +: tail.runtimeSeq

      lazy val valueSeq: Seq[Any] = runtimeSeq.map(_.compute)
    }

    implicitly[Eye =:= T0]
    implicitly[(Int ><: String ><: Eye) =:= (Int >< String)]

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
