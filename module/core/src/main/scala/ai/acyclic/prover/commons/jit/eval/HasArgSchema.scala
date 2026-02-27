package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.>:>
import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.Hom.Const
import ai.acyclic.prover.commons.tuple.{Products, Schemata}
import ai.acyclic.prover.commons.util.Phantom

trait HasArgSchema {

  import Args.><:

  type Args = Args.Prod
  type Arg0 = Args.Eye
  type Arg1[X] = X ><: Args.Eye
  type Args2[X, Y] = X ><: Y ><: Args.Eye

  object Args extends Schemata.Monoidal with Schemata.Cartesian_UID {

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
    sealed trait Prod extends SchemaMixin.Prod with Phantom {

      type Peer >: this.type <: Prod
      def peer: Peer

      type Top >: Peer <: Prod
      type Bottom <: Peer
      val Bottom: Bottom

      type ComputeAll <: TupleX.Prod

      type PayloadImpl <: Payload[Peer]

      /**
        * payload with all elements = [[Const.NotProvided]]
        */
      def noneProvidedPayload: Bottom.PayloadImpl
    }

    abstract class Payload[+S <: Prod](schema: S) {}

    override object eye extends Prod with SchemaMixin.Eye {

      type ComputeAll = TupleXEmpty

      override type Peer = this.type
      override def peer: Peer = this

      override type Top = this.type
      override type Bottom = this.type
      @transient override lazy val Bottom = this

      class PayloadImpl extends Payload(this)

      override def noneProvidedPayload: Bottom.PayloadImpl = new PayloadImpl()
    }

    infix type ><:[+H, T <: Prod] = Cons[? <: H, T]

    final infix class Cons[H, T <: Prod] private[Args] (
        val tail: T
    ) extends Prod
        with SchemaMixin.><:[H, T] {

      type ComputeAll = H *: tail.ComputeAll

      override type Peer = H ><: T
      override def peer: Peer = this

      override type Top = Any ><: T
      override type Bottom = Nothing ><: T
      @transient override lazy val Bottom: Bottom = {
        Cons[Nothing, T](tail)
      }

      class PayloadImpl(head: H, _tail: tail.PayloadImpl) extends Payload[Peer](peer)

      override def noneProvidedPayload: Bottom.PayloadImpl = {

        null.asInstanceOf[Bottom.PayloadImpl]
      }
    }

    object Cons {

      def apply[H, T <: Prod](tail: T): Cons[H, T] = new Cons[H, T](tail)
    }

    val v1: Int Cons T0 = ???
    implicitly[v1.Peer =:= (Int ><: T0)]

    implicitly[Eye =:= T0]
    implicitly[(Int ><: String ><: Eye) =:= (Int >< String)]
  }

}
