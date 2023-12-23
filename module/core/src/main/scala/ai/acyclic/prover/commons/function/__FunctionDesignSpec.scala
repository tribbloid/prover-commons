package ai.acyclic.prover.commons.function

object __FunctionDesignSpec {

  /**
    * advanced function API that allows referential transparency, function defined under it can be:
    *   - monomorphic, dependent or polymorphic (parametric or ad-hoc)
    *   - optionally, with named or nameless argument(s)
    *     - TODO: this is wildly infeasible due to high runtime overhead of `XX :: HNil`
    *     - TODO: should be made default once Args implementation is delegated to TupleSystem
    *   - analyzed and printed as an AST (required by static analysis)
    *   - enforced with chain rules (required by Bayesian inference and differential programming)
    *   - can be referential transparent, modified and perturbed (required by variational analysis)
    *   - eta-reduced or expanded to JVM method
    *     - (TODO: don't know how to do this yet)
    *
    * the following capabilities are postponed:
    *   - pure function
    *   - cached (required by memoization)
    */

  // Caching impl is delayed, should define Morphism first
}
