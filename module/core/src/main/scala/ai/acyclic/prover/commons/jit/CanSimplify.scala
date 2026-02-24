package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.{Args, PartialEvalEnv}

object CanSimplify extends CanSimplify_Impl0 {}

trait CanSimplify[+N <: IntermediateRepresentation] extends IntermediateRepresentation {

  type PartialEvalInputs <: Args
  lazy val noneProvided: PartialEvalInputs = throw new UnsupportedOperationException("PartialEvalInputs not provided")
  def partialEval(env: PartialEvalEnv[PartialEvalInputs]): N = {
    this.asInstanceOf[N] // safe by construction: concrete subclasses always extend CanSimplify[Self]
  }

  def simplify: N = {

//    val env = PartialEvalEnv(noneProvided, failFast = false, onlyPure = true)
    ////
    ////    partialEval(env)
    ???
  }
}
