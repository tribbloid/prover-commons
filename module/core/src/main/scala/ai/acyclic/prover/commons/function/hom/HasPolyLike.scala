package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capability
import ai.acyclic.prover.commons.function.Traceable
import ai.acyclic.prover.commons.util.SrcDefinition

trait HasPolyLike extends HasCircuit {

  trait PolyLike extends FromFunctionBuilder with Serializable with Traceable {

    object IsCase extends Capability

    type Case[+FF <: Fn.K2_[?, ?]] = FF <> IsCase.type

    type At[I] = Case[Fn.K2_[I, ?]]
    type Compat[-I, +O] = Case[Fn.K2_[I, O]]

    type Lemma[I, O] = Case[Fn.Impl[I, O]]
    object Lemma {

      type At[I] = Case[Fn.Impl[I, ?]]
    }

    type |-[I, O] = Lemma[I, O]
    // All lemma requires tightest In/Out type bound, like shapeless DepFn

    protected type Target[I, O] = Lemma[I, O]

    override def define[I, R](fn: I => R)(
        implicit
        _definedAt: SrcDefinition
    ): I Target R = {

      val _case = Fn(fn) <>: IsCase
      _case
    }

    implicit class AsSummoner[I, O](self: RefinedBuilder[I, O]) {

      def summon(
          implicit
          _case: Compat[I, O]
      ): _case.type = _case
    }

    object asShapelessPoly1 extends formless.hlist.Poly1 {

      implicit def rewrite[I, R](
          implicit
          _case: I |- R
      ): Case.Aux[I, R] = at[I] { v =>
        _case.apply(v)
      }
    }
  }
}
