package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.jit.eval.PartialEvalEnv

/**
  * FP stands for "Fixed-point", as in "F-bound"
  *
  * this is not the only way to represent inductive types:
  *   - Can be a type member but won't support variance
  *   - Can be a type member of outer type, but outer type has to be instantiated with all type arguments, not always
  *     practical if FP is generic
  */
trait CanSimplify[+FP <: CanSimplify[FP]] extends IntermediateRepresentation {

  def apply(arg: In): OutK[arg.type] // TODO: rename to eval

  /**
    * given complete or incmplete input, it should return a simplified/partially evaluated version of itself with best
    * effort.
    */
  def partialEval(env: () => PartialEvalEnv[In]): FP

  val simplify: FP
}

object CanSimplify {

  trait Elementary[+FP <: CanSimplify[FP]] extends CanSimplify[FP] {

    val noInput: In

    /**
      * simplifying is equivalent to partial evaluation with all inputs missing
      */
    private lazy val bottomEnvironment =
      PartialEvalEnv[In](inputs = noInput, failFast = false, onlyPure = true)

//    def partialEval(env: () => PartialEvalEnv[In]): FP

    @transient final lazy val simplify = partialEval(() => bottomEnvironment)
  }
}
