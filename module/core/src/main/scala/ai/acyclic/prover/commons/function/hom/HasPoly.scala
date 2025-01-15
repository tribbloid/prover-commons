package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.function.BuildTemplate
import ai.acyclic.prover.commons.util.SrcDefinition

import scala.language.implicitConversions

trait HasPoly extends HasPolyLike {

  /**
    * Ad-hoc polymorphic function, the most flexible polymorphism
    *
    * contains several cases, each take a type argument and generate a specific [[FnCompat]]
    *
    * the exact case being selected for function application should be determined in compile-time (by the implicit
    * evidence), doing it in runtime is shunned in type theories (it is fine in set theories tho), but we may still
    * allow it (if not obstructed by type erasure)
    *
    * obviously, both [[HasPoly1.Poly1]] and [[FnCompat]] are its trivial examples that only has 1 case
    */
  abstract class Poly(
      implicit
      override val _definedAt: SrcDefinition
  ) extends PolyLike
      with BuildTemplate {

    type BuildTarget[I, O] = Case[I, O]

    protected def build[I, O](fn: I => O)(
        implicit
        _definedAt: SrcDefinition
    ): BuildTarget[I, O] = {

      val _case = Fn.at[I](fn) <>: CaseTag
      _case
    }

    // TODO: all these cases can only be summoned when Poly is path-dependent, is there an API that works otherwise?

    def apply[I](v: I)(
        implicit
        _case: Lemma.At[I]
    ): _case._O = {
//      val revoked: FnCompat[v.type, R] = Capabilities.revokeAll[FnCompat[v.type, R], IsCase.type](_case)
//      val revoked: FnCompat[v.type, R] = Capabilities.revokeAll(_case)
      _case.apply(v)
    }

    case class BuildDomains[I, O]() extends IBuildDomains[I, O] {

      type _Lemma = Lemma[I, O]
      type _Impl = Case[I, O]
      type _Native = (I => O)

      def summon(
          implicit
          _case: Lemma[I, O]
      ): _case.type = _case
    }
    def refine[i, o]: BuildDomains[i, o] = BuildDomains()
  }

  object Poly {}

  implicit def asShapelessPoly1(v: Poly): v.asShapelessPoly1.type = v.asShapelessPoly1

  implicit class PolyOps[P <: Poly](self: P) extends Serializable {}
}
