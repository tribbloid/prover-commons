package ai.acyclic.prover.commons.jit.eval

/**
  * Partial evaluation environment
  *
  * @tparam PartialEvalInputs
  *   partial input type (Const[T]) or a tuple of several of them
  */
case class PartialEvalEnv[
    I <: Args
](
    inputs: I,
    failFast: Boolean, // used in function.apply, if false, will evaluate the graph with best effort
    onlyPure: Boolean // will not evaluate non-pure sub-functions
) {}

object PartialEvalEnv {

  trait AsConst[I] {

    type Out
  }

  object AsConst {

    infix type ~>[I, O] = AsConst[I] { type Out = O }
  }
}
