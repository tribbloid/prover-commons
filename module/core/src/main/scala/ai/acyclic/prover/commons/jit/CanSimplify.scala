package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.{Args, PartialEvalEnv}

object CanSimplify extends CanSimplify_Impl0 {}

trait CanSimplify[+N <: IntermediateRepresentation] extends IntermediateRepresentation {

  // TODO: the following 2 functons should be implemented in all subclasses
  //  given incmplete input "partialEval" should

  /**
    * given complete or incmplete input, it should return a simplified/partially evaluated version of itself with best
    * effort.
    */
  def partialEval(env: PartialEvalEnv[In]): N

  /**
    * simplifying is equivalent to partial evaluation with all inputs missing
    */
  val noneProvided: In
  final lazy val noneProvidedEnvironment: PartialEvalEnv[In] =
    PartialEvalEnv(noneProvided, failFast = false, onlyPure = true)

  final lazy val simplify: N = {

    partialEval(noneProvidedEnvironment)
  }
}
