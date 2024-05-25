package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.util.SrcPosition

trait FnBuilder {

  protected type =>>[i, _]

  def define[I, R](fn: I => R)(
      implicit
      _definedAt: SrcPosition
  ): I =>> R

  final def apply[I, R](fn: I => R)(
      implicit
      _definedAt: SrcPosition
  ): I =>> R = define(fn)

  case class RefinedBuilder[I, O]() {

    def at[i]: RefinedBuilder[i, O] = RefinedBuilder()

    def to[o]: RefinedBuilder[I, o] = RefinedBuilder()
    final def =>>[o]: RefinedBuilder[I, o] = to

    final def define[o <: O](fn: I => o)(
        implicit
        _definedAt: SrcPosition
    ): I =>> o = FnBuilder.this.define(fn)

    final def apply[o <: O](fn: I => o)(
        implicit
        _definedAt: SrcPosition
    ): I =>> o = define(fn)
  }

  // similar to `at` in shapeless Poly1
  def at[I]: RefinedBuilder[I, Any] = RefinedBuilder[I, Any]()

}
