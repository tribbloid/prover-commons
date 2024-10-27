package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capability
import ai.acyclic.prover.commons.util.SrcPosition

trait HasPolyLike extends HasCircuit {

  trait PolyLike extends FromFunctionBuilder {

    object IsCase extends Capability

    type WithCap[+FF <: Circuit._Compat[_, _]] = <>[FF, IsCase.type]

    type At[I] = WithCap[Circuit._Compat[I, _]]
    type CompatCase[-I, +O] = WithCap[Circuit._Compat[I, O]]

    type Lemma[I, O] = WithCap[Circuit.Impl[I, O]]
    type |-[I, O] = Lemma[I, O]
    type LemmaAt[I] = WithCap[Circuit.Impl[I, _]]
    // All lemma requires tightest In/Out type bound, like shapeless DepFn

    protected type Target[I, O] = WithCap[Circuit.Impl[I, O]]

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
          _case: CompatCase[I, O]
      ): _case.type = _case
    }
  }

}
