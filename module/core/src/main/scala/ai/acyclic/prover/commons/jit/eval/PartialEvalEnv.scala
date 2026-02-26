package ai.acyclic.prover.commons.jit.eval

/**
  * Partial evaluation environment. Stateful and should only be used once. Intermediate result should be cached by
  * function HashID and input
  *
  * @param inputs
  *   The "computeAll" value of it may contain both [[Const.Provided]] and [[Const.NotProvided]] part. They can be *
  *   used to partially evaluate [[ai.acyclic.prover.commons.jit.CanSimplify]] to make it simpler and faster.
  * @param failFast
  *   if false, will evaluate the graph with best effort
  *   - if true, will fail fast if any sub-function cannot be evaluated
  * @param onlyPure
  *   will avoid evaluating non-pure function components
  */
case class PartialEvalEnv[
    +I <: Args
](
    inputs: I,
    failFast: Boolean,
    onlyPure: Boolean
) {

  // TODO: implement the evaluation cachet
}

object PartialEvalEnv {

  trait AsConst[I] {

    type Out
  }

  object AsConst {

    infix type ~>[I, O] = AsConst[I] { type Out = O }
  }
}
