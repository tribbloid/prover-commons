package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.util.SrcPosition

trait FromFunctionBuilder {

  protected type Target[i, _]

  def define[I, R](fn: I => R)(
      implicit
      _definedAt: SrcPosition
  ): I Target R

  final def apply[I, R](fn: I => R)(
      implicit
      _definedAt: SrcPosition
  ): I Target R = define(fn)

  case class RefinedBuilder[I, O]() {

    def at[i]: RefinedBuilder[i, O] = RefinedBuilder()

    def to[o]: RefinedBuilder[I, o] = RefinedBuilder()
//    final def =>>[o]: RefinedBuilder[I, o] = to

    final def define[o <: O](fn: I => o)(
        implicit
        _definedAt: SrcPosition
    ): I Target o = FromFunctionBuilder.this.define(fn)

    final def apply[o <: O](fn: I => o)(
        implicit
        _definedAt: SrcPosition
    ): I Target o = define(fn)
  }

  // similar to `at` in shapeless Poly1
  def at[I]: RefinedBuilder[I, Any] = RefinedBuilder[I, Any]()

}
