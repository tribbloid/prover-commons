package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capability
import ai.acyclic.prover.commons.util.SrcPosition

trait HasPolyLike extends HasCircuit {

  trait PolyLike extends FromFunctionBuilder with Serializable {

    object IsCase extends Capability

    type Case[+FF <: Circuit.Theorem[?, ?]] = <>[FF, IsCase.type]

    type At[I] = Case[Circuit.Theorem[I, ?]]
    type Compat[-I, +O] = Case[Circuit.Theorem[I, O]]

    type Lemma[I, O] = Case[Circuit.Impl[I, O]]
    type |-[I, O] = Lemma[I, O]
    type LemmaAt[I] = Case[Circuit.Impl[I, ?]]
    // All lemma requires tightest In/Out type bound, like shapeless DepFn

    protected type Target[I, O] = Lemma[I, O]

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

    object asShapelessPoly1 extends shapeless.Poly1 {

      implicit def rewrite[I, R](
          implicit
          _case: I |- R
      ): Case.Aux[I, R] = at[I] { v =>
        _case.apply(v)
      }
    }
  }

}
