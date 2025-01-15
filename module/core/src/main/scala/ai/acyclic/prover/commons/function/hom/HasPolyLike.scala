package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.cap.Capability
import ai.acyclic.prover.commons.function.Traceable

trait HasPolyLike extends HasCircuit {

  trait PolyLike extends Serializable with Traceable {

    object CaseTag extends Capability

    type CaseTagged[+FF <: Fn[?, ?]] = FF <> CaseTag.type

    type Lemma[-I, +O] = CaseTagged[Fn[I, O]]
    object Lemma {
      type At[I] = CaseTagged[Fn[I, ?]]
    }
    type :=>[-I, +O] = Lemma[I, O]

    type Case[I, O] = CaseTagged[Fn.Impl[I, O]]
    object Case {
      type At[I] = CaseTagged[Fn.Impl[I, ?]]
    }
    type |-[I, O] = Case[I, O]

    // All lemma requires tightest In/Out type bound, like shapeless DepFn

//    override def define[I, R](fn: I => R)(
//        implicit
//        _definedAt: SrcDefinition
//    ): I Target R = {
//
//      val _case = Fn(fn) <>: IsCase
//      _case
//    }

//    implicit class AsSummoner[I, O](self: RefinedBuilder[I, O]) {
//
//      def summon(
//          implicit
//          _case: Compat[I, O]
//      ): _case.type = _case
//    }

    object asShapelessPoly1 extends formless.hlist.Poly1 {

      implicit def rewrite[I, R](
          implicit
          _case: I :=> R
      ): asShapelessPoly1.this.Case.Aux[I, R] = at[I] { v =>
        _case.apply(v)
      }
    }
  }
}
