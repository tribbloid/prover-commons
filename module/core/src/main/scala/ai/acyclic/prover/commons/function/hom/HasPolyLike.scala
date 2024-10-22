package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capability.<>
import ai.acyclic.prover.commons.util.SrcPosition

trait HasPolyLike extends HasCircuit {

  trait PolyLike extends FromFunctionBuilder {

    object IsCase extends HasCircuit.Capability

    type Case[+FF <: Circuit[_, _]] = <>[FF, IsCase.type]

    type At[I] = Case[Circuit[I, _]]
    type Compat[-I, +O] = Case[Circuit[I, O]]

    type Lemma[I, O] = Case[Circuit.Impl[I, O]]
    type LemmaAt[I] = Case[Circuit.Impl[I, _]]
    // All lemma requires tightest In/Out type bound, like shapeless DepFn

    protected type Target[I, O] = Case[Circuit.Impl[I, O]]

    override def define[I, R](fn: I => R)(
        implicit
        _definedAt: SrcPosition
    ): I Target R = {

      val _case = Circuit(fn) <>: IsCase
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
