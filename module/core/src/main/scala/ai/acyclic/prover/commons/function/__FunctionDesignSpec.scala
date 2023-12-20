package ai.acyclic.prover.commons.function

object __FunctionDesignSpec {

  /**
    * advanced function API that allows referential transparency, function defined under it can be:
    *   - monomorphic, dependent or polymorphic
    *   - with named or nameless argument(s)
    *   - analyzed and printed as an AST (required by static analysis)
    *   - enforced with chain rules (required by Bayesian inference and differential programming)
    *   - modified and perturbed (required by variational analysis)
    *   - eta-reduced or expanded to JVM method (TODO: don't know how to do this yet)
    *
    * the following capabilities are postponed:
    *   - pure function
    *   - cached (required by memoization)
    */

  // Caching impl is delayed, should define Morphism first
}
