package ai.acyclic.prover.commons.jit.eval

import ai.acyclic.prover.commons.compat.{*:, TupleX, TupleXEmpty}
import ai.acyclic.prover.commons.jit.Hom
import ai.acyclic.prover.commons.jit.Hom.Const
import ai.acyclic.prover.commons.tuple.{Products, Schemata}

trait HasArgs {

  trait Contra[-T]

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

      type Union <: Contra[?]

      type Peer >: this.type <: Prod
      def peer: Peer

      type Top >: Peer <: Prod
      type _NoInput <: Peer & Args.NoInputK[?]
    }

    implicit class ProdExt[T <: Prod & NoInputK[T]](self: T) {

      def consNoInput = new ConsNoInput(self)
    }

    type NoInput = Prod & NoInputK[?]

    object NoInput {

      type T0 = Eye
      def T0 = Args.Eye

      type T1 = ConsNoInput[Eye]
      def T1 = Const.NotProvided ><: Args.Eye

      type T2 = ConsNoInput[ConsNoInput[Eye]]
      def T2 = Const.NotProvided ><: Const.NotProvided ><: Args.Eye

      implicitly[T0 <:< NoInput]
      implicitly[T1 <:< NoInput]
      implicitly[T2 <:< NoInput]
    }

    trait NoInputK[-F <: Args] extends Args {
      // TODO: what if the F-bound is not required

      type Union >: Contra[Nothing] <: Contra[?] // AKA type Union = Contra[? <: Nothing]
    }

    sealed trait eyeLike extends Prod with ElementsMixin.Eye with NoInputK[eyeLike] {

      type ComputeAll = TupleXEmpty
      override lazy val computeAll: ComputeAll = TupleXEmpty

      override type Union = Contra[Nothing]

      override type Peer = this.type
      override def peer: Peer = this

      override type Top = this.type
      override type _NoInput = this.type

      type BA = Eye
    }

    override object eye extends eyeLike

    type ><:[+H, +T <: Prod] = Cons[? <: H, ? <: T]

    protected case class Cons[H, T <: Prod] private[Args] (
        val head: Element[H],
        val tail: T
    ) extends Prod
        with ElementsMixin.><:[H, T] {

      type ComputeAll = H *: tail.ComputeAll
      override lazy val computeAll: ComputeAll = computeHead *: tail.computeAll

      def computeHead: H = head.compute

      type Union = Contra[H] & tail.Union // equivalent to Contra[H | TU] where tail.Union = Contra[TU]

      override lazy val runtimeSeq = head +: tail.runtimeSeq

      lazy val valueSeq: Seq[Any] = runtimeSeq.map(_.compute)

      override type Peer = H ><: T
      override def peer: Peer = this

      override type Top = Any ><: T
      override type _NoInput = ConsNoInput[tail._NoInput] & Peer
    }

    class ConsNoInput[T <: NoInput](tail: T)
        extends Cons[Nothing, T](Const.NotProvided, tail)
        with NoInputK[Nothing ><: T]

    implicitly[Eye =:= T0]
    implicitly[(Int ><: String ><: Eye) =:= (Int >< String)]

    override protected def cons[L, TAIL <: Prod](head: Element[L], tail: TAIL): L ><: TAIL =
      new Cons(head, tail)

    override def deCons[L, TAIL <: Prod](cons: L ><: TAIL): (Element[L], TAIL) =
      (cons.head, cons.tail)

  }

}
