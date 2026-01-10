package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.tracing.Tracer.Gen

trait TracerImplicits extends TracerCanChain {

  implicit def _getValue[T](v: Tracer[?, T])(
      implicit
      position: SrcDefinition = null
  ): T =
    v.getValue

  implicit class UnaryView[I, O](private val self: Tracer[I, O]) {

    // minimal requirement for for-comprehension
    def map[OO](right: Var[O] => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Tracer[I, canChain.Repr] = {
      ???
    }

    def foreach(right: Var[O] => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: Var[O] => Tracer[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Tracer[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: Var[O] => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[I, O] = {

      ???
    }
  }

  implicit class BinaryView[I, O1, O2](private val self: Tracer[I, (O1, O2)]) {
    // TODO: should it be of higher implicit tier?

    // minimal requirement for for-comprehension
    def map[OO](right: ((Var[O1], Var[O2])) => Gen[OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Tracer[I, canChain.Repr] = {
      ???
    }

    def foreach(right: ((Var[O1], Var[O2])) => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: ((Var[O1], Var[O2])) => Tracer[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Tracer[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: ((Var[O1], Var[O2])) => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[I, (O1, O2)] = {
      ???
    }
  }
}
