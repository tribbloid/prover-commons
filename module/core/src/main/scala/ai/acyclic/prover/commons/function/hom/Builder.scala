package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

trait Builder {

  protected type =>>[i, _]

  def define[I, R](fn: I => R): I =>> R

  final def apply[I, R](fn: I => R): I =>> R = define(fn)

  case class RefinedBuilder[I, O]() {

    def at[i]: RefinedBuilder[i, O] = RefinedBuilder()

    def to[o]: RefinedBuilder[I, o] = RefinedBuilder()
    final def =>>[o]: RefinedBuilder[I, o] = to

    final def define[o <: O](fn: I => o): I =>> o = Builder.this.define(fn)

    final def apply[o <: O](fn: I => o): I =>> o = define(fn)
  }

  // similar to `at` in shapeless Poly1
  def at[I]: RefinedBuilder[I, Any] = RefinedBuilder[I, Any]()

  implicit def definedHere: CallStackRef = CallStackRef
    .below(condition = { v =>
      v.isDefinedAtClasses(classOf[HasFn]) || v.isArgDefault
    })
    .below(1)
}
