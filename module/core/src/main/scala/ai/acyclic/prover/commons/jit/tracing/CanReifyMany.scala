package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

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

  /**
    * may fail with [[ConcretizationTypeError]], which is why [[SrcDefinition]] is required
    */
  def reify(
      inputs: I
  )(
      implicit
      defAt: SrcDefinition
  ): Out

  /**
    * never fails
    */
  def const(
      values: Out
  ): I
}

object CanReifyMany extends CanReifyMany_Imp0 {

  infix type Aux[T, O] = CanReifyMany[T] { type Out = O }

  implicit def atom[O]: Aux[Input[O], O] = new CanReifyMany[Input[O]] {
    type Out = O
    override def reify(inputs: Input[O])(
        implicit
        defAt: SrcDefinition
    ): O = inputs.reify
    override def const(values: O): Input[O] = Const(values)
  }

  implicit val unit: Aux[Unit, Unit] = new CanReifyMany[Unit] {
    type Out = Unit

    override def reify(inputs: Unit)(
        implicit
        defAt: SrcDefinition
    ): Unit = ()

    override def const(values: Unit): Unit = ()
  }
}
