package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.tuple.Schemata

import scala.util.Try

trait HasArgSchema {

  import Args.><:

  type Args = Args.Prod
  type Arg0 = Args.Eye
  type Arg1[X] = X ><: Args.Eye
  type Args2[X, Y] = X ><: Y ><: Args.Eye

  object Args extends Schemata.Monoidal with Schemata.Cartesian_UID {

    override type VBound = Any

    override type Element[V] = Hom.ConstantFn[V]

    /**
      * Schema-only phantom class that contains no data.
      *
      * To construct
      */
    sealed trait Prod extends SchemaMixin.Prod {

      type Peer >: this.type <: Prod
      def peer: Peer

      type Top >: Peer <: Prod
      type Bottom <: Peer
      val Bottom: Bottom

      type TryComputeAll <: TupleX.Prod
      type ComputeAll <: TupleX.Prod

      abstract class _Payload extends Payload(Prod.this) {

        val tryComputeAll: TryComputeAll
        val computeAll: ComputeAll
      }
      type PayloadImpl <: _Payload

      /**
        * payload with all elements = [[Const.NotProvided]]
        */
      def noneProvidedPayload: Bottom.PayloadImpl
    }

    abstract class Payload[+S <: Prod](schema: S) {}

    override object eye extends Prod with SchemaMixin.Eye {

      type TryComputeAll = TupleXEmpty
      type ComputeAll = TupleXEmpty

      override type Peer = this.type
      override def peer: Peer = this

      override type Top = this.type
      override type Bottom = this.type
      @transient override lazy val Bottom = this

      class PayloadImpl extends _Payload

      override def noneProvidedPayload: Bottom.PayloadImpl = new PayloadImpl()
    }

    infix type ><:[+H, T <: Prod] = Cons[? <: H, T]

    final infix class Cons[H, T <: Prod] private[Args] (
        val tail: T
    ) extends Prod
        with SchemaMixin.><:[H, T] {

      type TryComputeAll = Try[H] *: tail.TryComputeAll
      type ComputeAll = H *: tail.ComputeAll

      override type Peer = H ><: T
      override def peer: Peer = this

      override type Top = Any ><: T
      override type Bottom = Nothing ><: T
      @transient override lazy val Bottom: Bottom = {
        Cons[Nothing, T](tail)
      }

      class PayloadImpl(head: H, _tail: tail.PayloadImpl) extends _Payload

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
