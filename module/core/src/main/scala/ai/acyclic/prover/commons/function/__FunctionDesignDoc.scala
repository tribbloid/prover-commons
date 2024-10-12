package ai.acyclic.prover.commons.function

object __FunctionDesignDoc {

  /**
    * advanced function API that allows referential transparency, function defined under it can be:
    *   - monomorphic, dependent or polymorphic (parametric or ad-hoc)
    *   - optionally, with named or nameless argument(s)
    *     - TODO: this is wildly infeasible due to high runtime overhead of `XX :: HNil`
    *     - TODO: should be made default once Args implementation is delegated to 0-overhead TupleSystem
    *   - analyzed and printed as an AST (required by static analysis)
    *   - transformed with chain rules or other functor laws (required by Bayesian inference and differential
    *     programming)
    *   - can be referential transparent, modified and perturbed (required by variational analysis)
    *   - eta-reduced or expanded to JVM method
    *     - (TODO: don't know how to do this yet)
    *   - cached (required by memoization)
    *
    * the following capabilities are postponed:
    *   - pure function (can it be an effect un-lifted by validating once, like
    *     [[ai.acyclic.prover.commons.cap.Pending.OrNull]]?)
    *   - axiomatic composition: the implicit search of Scala 2 is not strong enough to support it
    *
    * Most verbosity in definition came from bounded HKT (higher kinded type):
    *
    * F[_ <: /\] requires 2 arguments (/\ and F) that are too bloat to define
    *
    * it also causes divergence between [[Hom.Mono]] and [[Hom.Poly]]
    *
    * One solution is to brake it into a polymorph and a dependent type, but type class relies heavily on unreliable
    * implicit.
    *
    * Is there a "reliable" implicit for polymorph? This is a world class problem: unlike function or dependent type,
    * implicit scope (representing a collection of axioms) cannot be assigned to a symbol
    *
    * fn[T <: TUB](): R {type Out = ...}
    */

  /**
    * glossary:
    *
    *   - Expr - can be a Fn / Const / Thunk
    *   - Const/Thunk can preserve its computation graph
    *   - ^(v: Any): Const - lifting, value/Fn can be lifted to represent a Const in the next stage
    *   - (fn: Fn).^(v: Expr): Thunk - suspending: define a Thunk that is yet to be computed
    *   - (fn: Fn).andThen(fn2: Fn): Fn - composing, equivalent to {v => fn2(fn(v))}
    *   - (v: Const|Thunk).get(): Any - compute/get its value
    *   - (v: Expr).map(fn2: O => O2): v.PeerType - continuation, create another Expr where fn2 is applied to the result
    *     of this Expr
    *     - alternative syntax is for (o: O <- v: Expr) yield { o2: O2 }
    */

  /**
    * after revision:
    *
    *   - `I :=> O` becomes Const[I => O]
    *     - (I => O) won't contain computation graph
    */

  /**
    * TODO: (09/30/2024)
    *
    *   - Tracer should be discarded - no need to fit into the ecosystem of JAX as it relies heavily on capability of
    *     Python
    *   - JAX tracer is always for bound variables that cannot exist without a function body. Free variables are never
    *     traced
    *   - the optimal representation for variables is DeBruijn number, which also cannot exist without a functino body.
    *   - unlike Python, Scala (or other strongly typed languages) functions don't have a "shadow mode" that can be
    *     applied on tracer (e.g. dual numbers)
    *     - the only Scala-native option is to use AST transformation.
    *     - examples of AST transformations are Kyo direct-style and dotty-cps-async
    */

  /**
    * TODO: (10/05/2024)
    *
    * In JAX:
    *   - const / thunk / nullary function are not traced for 1st-order functions, no need
    *     - but it should be traced for higher-order functions! Even if it is a constant, it may affect the grad of
    *       other variables!
    *     - as a result, it should always be introduced as a const
    *
    * Unfortunately in Scala, we have 2 competing methods to define expressions (& their axiomatic rewrite rules):
    *
    *   - define-by-run (preferred by all Python-based DL libraries & egglog-python):
    *     - impl in rise compiler that uses G-EqSat
    *
    *   - define-by-rewrite (a.k.a. staged compile) (preferred by catgrad):
    *     - compile-time debugging is hard!
    *     - therefore, development will start with an API (called "indirect/comprehension-style") that defines the
    *       program AFTER the rewrite
    *     - ... then add the intended rewrite capability (called "direct-style") that works in both runtime (for
    *       debugging) & compile-time (for performance)
    *       - in Scala, it usually manifest as for-comprehension, impl in Kyo, dotty-cps-async
    *
    * Both are complex. In best case I only need to impl one of them, if the G-EqSat impl doesn't offer anything new
    *
    * also, function.apply() in define-by-run shouldn't get special treatment, it is like other operators (+, *)
    *
    * the define-by-run API will be postponed, as ArithExpr is hyper-optimised for SIMD linear algebra, the more general
    * G-EqSat impl won't rely on it
    */
}
