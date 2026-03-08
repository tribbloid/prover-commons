package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.Hom.Const
import ai.acyclic.prover.commons.tuple.{Products, Schemata}
import ai.acyclic.prover.commons.util.Phantom

import scala.util.Try

trait HasArgs {

  import Args.><:

  /**
    * any number of function arguments, always right-associative.
    *
    *   - element/tryCompute method for partial eval
    *   - compute for direct eval
    */
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
    type Schema[P <: Args] = Schema.KK { type Peer = P }

    object Schema {

      type T0 = Eye
      type T1[X] = Cons[X, T0]
      type T2[Y, X] = Cons[Y, Cons[X, T0]]

      sealed trait KK extends Phantom with Serializable {

        type Peer <: Args
        type Top >: Peer <: Args
        type Bottom <: Peer

        type TryComputeAll <: TupleX.Prod
        type ComputeAll <: TupleX.Prod

        def bottom: Bottom

        final def cons[H] = new Cons[H, this.type](this)
      }

      type Gt[T <: Args] = KK { type Peer >: T <: Args }

      object Eye extends KK {

        type Peer = Args.T0
        type TryComputeAll = TupleXEmpty
        type ComputeAll = TupleXEmpty

        override type Top = Args.T0
        override type Bottom = Args.T0

        override def bottom: Bottom = Args.T0
      }
      type Eye = Eye.type

      final infix case class Cons[H, T <: KK] private[Schema] (
          tail: T
      ) extends KK {

        type Peer = H ><: tail.Peer
        type TryComputeAll = Try[H] *: tail.TryComputeAll
        type ComputeAll = H *: tail.ComputeAll

        override type Top = Any ><: tail.Top
        override type Bottom = Nothing ><: tail.Bottom

        override def bottom: Bottom = Const.NotProvided ><: tail.bottom
      }

      object WildCard extends KK {

        type Peer = Args
        type TryComputeAll = TupleX
        type ComputeAll = TupleX

        override type Top = Args
        override type Bottom = Nothing

        override def bottom: Bottom = throw new IllegalAccessError("WildCard doesn't have a bottom element")
      }
    }

    sealed trait Prod extends ElementsMixin.Prod {

      val schema: Schema[?]

      val computeAll: schema.ComputeAll
    }

    override object eye extends Prod with ElementsMixin.Eye {

      lazy val schema: Schema.Eye = Schema.Eye

      override lazy val computeAll: schema.ComputeAll = TupleXEmpty

    }

    type ><:[+H, +T <: Prod] = Cons[? <: H, ? <: T]

    final protected case class Cons[H, T <: Args] private[Args] (
        head: Element[H],
        tail: T
    ) extends Prod
        with ElementsMixin.><:[H, T] {

      lazy val schema: Schema.Cons[H, tail.schema.type] = tail.schema.cons[H]

      override lazy val computeAll: schema.ComputeAll = {

        val result = head.compute *: tail.computeAll
        val result2: H *: tail.schema.ComputeAll = result

        result
      }

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

  trait NoneGenerator[-T <: (Option[String], Option[Int])] {

    val bottom = (None, None)

    def gen[R >: (Option[Nothing], Option[Nothing]) <: T]: R =
      bottom // it is possbile to output value with contravariant bound
  }

}
