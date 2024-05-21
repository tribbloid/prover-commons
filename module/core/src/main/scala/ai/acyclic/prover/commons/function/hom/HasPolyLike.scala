package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capability.<>

trait HasPolyLike extends HasFn {

  import Explainable._

  trait PolyLike extends Builder with Explainable {

    object IsCase extends Explainable.Capability

    type Case[+FF <: FnCompat[_, _]] = <>[FF, IsCase.type]

    type At[I] = Case[FnCompat[I, _]]
    type Compat[I, O] = Case[FnCompat[I, O]]
    type =>>[I, O] = Case[FnImpl[I, O]]

    override def define[I, R](fn: I => R): I =>> R = {

      val _case = Fn(fn) <>: IsCase
      _case
    }

    implicit class BuildExtension[I, O](self: RefinedBuilder[I, O]) {

      def summon(
          implicit
          _case: Compat[I, O]
      ): _case.type = _case
    }
  }
}
