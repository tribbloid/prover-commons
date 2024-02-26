package ai.acyclic.prover.commons.jit

/**
  * This capabilites seek to unify function apply/eval and simplify, both are perceived as special cases:
  *
  *   - apply/eval = partial evaluation with all inputs provided while treating all part of the function as pure
  *   - simplify = partial evaluation with none of the input provided
  *
  * in both cases and every cases in-between, it can convert part of a function into simpler form or values
  *
  * for Fusing programs, it is useful in simplifying every programs into a normal form, then use its computation graph
  * to determine sameness/fusability
  *
  * BE CAUTIOUS in using this to optimise programs:
  *
  *   - partial evaluation only accelerate the pure part of a functions
  *   - partial evaluation is random and best-effort, some input (e.g. Lazy) may fail randomly, multiple attempts may
  *     gradually make a function faster and simpler
  *
  * This is an advanced topic that may only become useful much later. But for implementing, it may not be much harder
  * than simplify
  *
  * TODO:
  *
  *   - [ ] impl `partialEval: EvalEnv => Fn` function for Fn (same type args). EvalEnv contains cache to ensure that
  *     same sub Fn always yield same Const
  *   - [ ] impl `specialise`: if the function is pure, use partialEval, otherwise keep itself.
  *   - [ ] impl `apply`: partialEval but fail early
  */
object __PartialEvaluationRequirements {}
