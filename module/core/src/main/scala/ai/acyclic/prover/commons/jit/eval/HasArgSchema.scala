package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.Hom.Const
import ai.acyclic.prover.commons.tuple.{Products, Schemata}

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
    sealed trait Prod extends SchemaMixin.Prod {

      type Peer <: Prod
      type Top >: Peer <: Prod
      type Bottom <: Peer
      val Bottom: Bottom

      type ComputeAll <: TupleX.Prod

      type _Payload <: Payload[Peer]
    }

    abstract class Payload[S <: Prod](schema: S) {}

    override object eye extends Prod with SchemaMixin.Eye {

      type ComputeAll = TupleXEmpty

      override type Peer = this.type
      override type Top = this.type
      override type Bottom = this.type
      override val Bottom = this

      class _Payload extends Payload(this)
    }

    type ><:[+H, +T <: Prod] = Cons[? <: H, T]

    case class Cons[H, +T <: Prod] private[Args] (
        tail: T
    ) extends Prod
        with SchemaMixin.><:[H, T] {

      type ComputeAll = H *: tail.ComputeAll

      override type Peer = H ><: T
      override type Top = Any ><: tail.Top
      override type Bottom = Nothing ><: tail.Bottom
      override lazy val Bottom: Bottom = {
        Cons(tail.Bottom)
      }

      class _Payload extends Payload[Peer](this)
    }

    implicitly[Eye =:= T0]
    implicitly[(Int ><: String ><: Eye) =:= (Int >< String)]
  }

}
