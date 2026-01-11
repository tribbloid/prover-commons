package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

trait ConstructorImplicits extends ConstructorCanChain {

  implicit class UnaryView[I, O](private val self: Constructor[I, O]) {

    // minimal requirement for for-comprehension
    def map[OO](right: Var[O] => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Constructor[I, canChain.Repr] = {
      ???
    }

    def foreach(right: Var[O] => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): Constructor[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: Var[O] => Constructor[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Constructor[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: Var[O] => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): Constructor[I, O] = {

      ???
    }
  }

  implicit class BinaryView[I, O1, O2](private val self: Constructor[I, (O1, O2)]) {
    // TODO: should it be of higher implicit tier?

    // minimal requirement for for-comprehension
    def map[OO](right: ((Var[O1], Var[O2])) => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Constructor[I, canChain.Repr] = {
      ???
    }

    def foreach(right: ((Var[O1], Var[O2])) => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): Constructor[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: ((Var[O1], Var[O2])) => Constructor[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Constructor[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: ((Var[O1], Var[O2])) => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): Constructor[I, (O1, O2)] = {
      ???
    }
  }
}
