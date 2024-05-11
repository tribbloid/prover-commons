package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.cap.Capability.<>
import ai.acyclic.prover.commons.debug.Debug.CallStackRef

trait HasPolyLike extends HasFn {

  import Explainable._

  trait PolyLike extends Fn.CanBuild with Explainable {

    object IsCase extends Explainable.Capability

    type Case[+FF <: Fn[_]] = <>[FF, IsCase.type]

    type At[I <: IUB] = Case[Fn[I]]
    type Compat[I <: IUB, O] = Case[FnCompat[I, O]]
    type =>>[I <: IUB, O] = Case[FnImpl[I, O]]

    override protected def _defining[I <: IUB, R](fn: I => R)(
        implicit
        _definedAt: CallStackRef = definedHere
    ): I =>> R = {

      val _case = Fn(fn) <>: IsCase
      _case
    }

    implicit class BuildExtension[I <: IUB, O](self: Builder[I, O]) {

      def summon(
          implicit
          _case: Compat[I, O]
      ): _case.type = _case
    }

  }
}
