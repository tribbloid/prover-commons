package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capability.<>
import ai.acyclic.prover.commons.util.{SrcExplainable, SrcPosition}

trait HasPolyLike extends HasFn {

  import ai.acyclic.prover.commons.util.SrcExplainable._

  trait PolyLike extends SrcExplainable with FnBuilder {

    object IsCase extends SrcExplainable.Capability

    type Case[+FF <: FnCompat[_, _]] = <>[FF, IsCase.type]

    type At[I] = Case[FnCompat[I, _]]
    type Compat[I, O] = Case[FnCompat[I, O]]
    type =>>[I, O] = Case[FnImpl[I, O]]

    override def define[I, R](fn: I => R)(
        implicit
        _definedAt: SrcPosition
    ): I =>> R = {

      val _case = Fn(fn) <>: IsCase
      _case
    }

    implicit class AsSummoner[I, O](self: RefinedBuilder[I, O]) {

      def summon(
          implicit
          _case: Compat[I, O]
      ): _case.type = _case
    }
  }

}
