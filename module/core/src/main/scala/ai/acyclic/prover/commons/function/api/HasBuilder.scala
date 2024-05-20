package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.debug.Debug.CallStackRef

import scala.language.implicitConversions

trait HasBuilder {

  /**
    * will be Tuple-like in the future
    */
  type IUB // TODO: should be "Domain"

  trait Builder {

    protected type =>>[i <: IUB, _]

    def define[I <: IUB, R](fn: I => R)(
        implicit
        _definedAt: CallStackRef = definedHere
    ): I =>> R

    final def apply[I <: IUB, R](fn: I => R)( // alias
        implicit
        _definedAt: CallStackRef = definedHere
    ): I =>> R = define(fn)

    case class RefinedBuilder[I <: IUB, O]() {

      def at[i <: IUB]: RefinedBuilder[i, O] = RefinedBuilder()

      def to[o]: RefinedBuilder[I, o] = RefinedBuilder()
      final def =>>[o]: RefinedBuilder[I, o] = to

      final def define[o <: O](fn: I => o)(
          implicit
          _definedAt: CallStackRef = definedHere
      ): I =>> o = Builder.this.define(fn)

      final def apply[o <: O](fn: I => o)( // alias
          implicit
          _definedAt: CallStackRef = definedHere
      ): I =>> o = define(fn)
    }

    // similar to `at` in shapeless Poly1
    def at[I <: IUB]: RefinedBuilder[I, Any] = RefinedBuilder[I, Any]()
  }

  protected[function] def definedHere: CallStackRef = CallStackRef
    .below(condition = { v =>
      v.isDefinedAtClasses(classOf[HasFn]) || v.isArgDefault
    })
    .below(1)
}
