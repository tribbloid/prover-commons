package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.util.TupleUnpack
import zio.Zippable

/**
  * Typeclass to convert a tuple of [[Input]] into their corresponding values (using their (reify) function)
  *
  * this conversion is achieved by recursively summoning itself, thus applicable to tuple of any size
  *
  * e.g. (Input[X], Input[Y], Input[Z]) => (X, Y, Z)
  *
  * Implementation should use [[ai.acyclic.prover.commons.util.TupleUnpack]] for the recursion. Do not use shapeless
  * directly. Make sure all tests are successful.
  */
trait CanReifyMany[
    I // (Input[X], Input[Y], ...)
] {

  type Out // (X, Y, ...)

  def reify(
      inputs: I
  )(
      implicit
      defAt: SrcDefinition
  ): Out

  def const(
      values: Out
  ): I
}

object CanReifyMany extends CanReifyMany_Imp0 {

  type Aux[T, O] = CanReifyMany[T] { type Out = O }

//  implicit def atom[O]: Aux[Input[O], O] = new CanReifyMany[Input[O]] {
//    type Out = O
//    override def reifyMany(inputs: Input[O])(
//        implicit
//        defAt: SrcDefinition
//    ): O = inputs.reify
//  } // TODO: remove

  implicit val unit: Aux[Unit, Unit] = new CanReifyMany[Unit] {
    type Out = Unit

    override def reify(inputs: Unit)(
        implicit
        defAt: SrcDefinition
    ): Unit = ()

    override def const(values: Unit): Unit = ()
  }
}

trait CanReifyMany_Imp0 {

  implicit def unpack[
      T,
      Head,
      Tail,
      HO,
      TLO,
      O
  ](
      implicit
      unpack: TupleUnpack.Aux[T, Head, Tail],
      ev: Head <:< Input[HO],
      tReify: CanReifyMany.Aux[Tail, TLO],
      zippable: Zippable.Out[HO, TLO, O]
  ): CanReifyMany.Aux[T, O] = new CanReifyMany[T] {
    type Out = O

    override def reify(inputs: T)(
        implicit
        defAt: SrcDefinition
    ): O = {
      val (hRaw, t): (Head, Tail) = unpack.unpack(inputs)
      val h: Input[HO] = ev(hRaw)
      val ho = h.reify(defAt)
      val tlo = tReify.reify(t)
      zippable.zip(ho, tlo)
    }

    override def const(values: O): T = {
      ???
    }
  }
}
