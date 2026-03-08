package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.PartialEvalEnv

object CanSimplify extends CanSimplify_Impl0 {}

trait CanSimplify[+N <: IntermediateRepresentation] extends IntermediateRepresentation {
  self: N =>

  def apply(arg: In): OutK[arg.type] // TODO: rename to eval

  /**
    * given complete or incmplete input, it should return a simplified/partially evaluated version of itself with best
    * effort.
    */
  def partialEval(env: () => PartialEvalEnv[In]): N

  /**
    * simplifying is equivalent to partial evaluation with all inputs missing
    */
  final lazy val bottomEnvironment =
    PartialEvalEnv[In](inputs = inputSchema.bottom, failFast = false, onlyPure = true)

  final lazy val simplify: N = {

    partialEval(() => bottomEnvironment)
  }
}
