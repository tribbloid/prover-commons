package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.PartialEvalEnv

object CanSimplify extends CanSimplify_Impl0 {}

trait CanSimplify[+N <: IntermediateRepresentation] extends IntermediateRepresentation {
  self: N =>

  final private val MissingInputSchemaMessage = "inputSchema is unavailable for generic DepFn"

  private lazy val unavailableInputs: In =
    throw new UnsupportedOperationException("bottom inputs are unavailable for generic DepFn")

  private lazy val fallbackEnvironment: PartialEvalEnv[In] =
    PartialEvalEnv[In](inputs = unavailableInputs, failFast = false, onlyPure = true)

  def apply(arg: In): OutK[arg.type] // TODO: rename to eval

  /**
    * given complete or incmplete input, it should return a simplified/partially evaluated version of itself with best
    * effort.
    */
  def partialEval(env: PartialEvalEnv[In]): N

  /**
    * simplifying is equivalent to partial evaluation with all inputs missing
    */
  final lazy val bottomEnvironment =
    PartialEvalEnv[In](inputs = inputSchema.bottom, failFast = false, onlyPure = true)

  final lazy val simplify: N = {

    try {
      partialEval(bottomEnvironment)
    } catch {
      case ex: UnsupportedOperationException if ex.getMessage == MissingInputSchemaMessage =>
        try {
          partialEval(fallbackEnvironment)
        } catch {
          case _: UnsupportedOperationException =>
            this
        }
    }
  }
}
