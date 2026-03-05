package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.Hom.Const
import ai.acyclic.prover.commons.tuple.{Products, Schemata}
import ai.acyclic.prover.commons.util.Phantom

import scala.util.Try

trait HasArgSchema {

//  import ArgSchema.><:

//  type Args = ArgSchema.Prod
//  type Arg0 = ArgSchema.Eye
//  type Arg1[X] = X ><: ArgSchema.Eye
//  type Args2[X, Y] = X ><: Y ><: ArgSchema.Eye

  object ArgSchema extends Schemata.Monoidal with Schemata.Cartesian_UID {

    import TupleX.*

    override type VBound = Any

    override type Element[V] = Unit

    /**
      * Schema-only phantom class that contains no data.
      *
      * To construct
      */
    sealed trait Prod extends SchemaMixin.Prod with Phantom {

      type Peer >: this.type <: Prod

      type Top >: Peer <: Prod
      type Bottom <: Peer

      type TryComputeAll <: TupleX.Prod
      type ComputeAll <: TupleX.Prod

//      abstract class _Payload extends Payload(Prod.this) {
//
//        val tryComputeAll: TryComputeAll
//        val computeAll: ComputeAll
//      }
//      type PayloadImpl <: _Payload
    }

    abstract class Payload[+S <: Prod](schema: S) {}

    override object eye extends Prod with SchemaMixin.Eye {

      type TryComputeAll = TupleXEmpty
      type ComputeAll = TupleXEmpty

      override type Peer = this.type

      override type Top = this.type
      override type Bottom = this.type
    }

    infix type ><:[+H, T <: Prod] = Cons[? <: H, T]

    final infix class Cons[H, T <: Prod] private[ArgSchema] () extends Prod with SchemaMixin.><:[H, T] {

      lazy val tail: T = Phantom.apply()

      type TryComputeAll = Try[H] *: tail.TryComputeAll
      type ComputeAll = H *: tail.ComputeAll

      override type Peer = H ><: T

      override type Top = Any ><: T
      override type Bottom = Nothing ><: T

//      class PayloadImpl(head: Element[H], _tail: tail.PayloadImpl) extends _Payload {
//
//        override lazy val tryComputeAll: TryComputeAll = Try(head.compute) *: _tail.tryComputeAll
//        override lazy val computeAll: ComputeAll = head.compute *: _tail.computeAll
//      }
//
//      override def noneProvidedPayload: PayloadImpl =
//        new PayloadImpl(Const.NotProvided, tail.noneProvidedPayload)
    }

//    object Cons {
//
//      def apply[H, T <: Prod](tail: T): Cons[H, T] = new Cons[H, T](tail)
//    }

  }

}
