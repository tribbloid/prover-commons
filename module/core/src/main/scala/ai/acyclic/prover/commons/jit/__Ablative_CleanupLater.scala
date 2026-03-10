package ai.acyclic.prover.commons.jit

object __Ablative_CleanupLater {

  trait PartialEvalEnv[+T]

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

    trait Elementary[+FP <: Elementary[FP]] extends CanSimplify[FP & Elementary[FP]] {

      val noInput: In

      /**
        * simplifying is equivalent to partial evaluation with all inputs missing
        */
      private lazy val bottomEnvironment =
        new PartialEvalEnv[In] {}

      //    def partialEval(env: () => PartialEvalEnv[In]): FP & Elementary[FP]

      @transient final lazy val simplify = partialEval(() => bottomEnvironment)
    }
  }
}
