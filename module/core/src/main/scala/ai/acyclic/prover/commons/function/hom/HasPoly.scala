package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.tag.Tag
import ai.acyclic.prover.commons.tag.<>
import ai.acyclic.prover.commons.compat.TupleX
import ai.acyclic.prover.commons.function.FnBuilder
import ai.acyclic.prover.commons.debug.SrcDefinition

trait HasPoly extends HasPolyLike {

  /**
    * Ad-hoc polymorphic function, the most flexible polymorphism
    *
    * contains several cases, each take a type argument and generate a specific [[Case]]
    *
    * the exact case being selected for function application should be determined in compile-time (by the implicit
    * evidence), doing it in runtime is shunned in type theories (it is fine in set theories tho), but we may still
    * allow it (if not obstructed by type erasure)
    */
  abstract class Poly(
      implicit
      override val _definedAt: SrcDefinition
  ) extends PolyLike
      with FnBuilder.Root {

    object CaseTag extends Tag

    type Case[+FF <: Fn[?, ?]] = FF <> CaseTag.type

    type Lemma[-I, +O] = Case[Fn[I, O]] // antecedent, compatibility type at logical consuming site, hence the name
    object Lemma {
      type At[I] = Case[Fn[I, ?]]
    }
    type :=>[-I, +O] = Lemma[I, O]

    type Impl[I, O] = Case[Fn.Impl[I, O]] // consequent, most specific type at logical producing site
    object Impl {
      type At[I] = Case[Fn.Impl[I, ?]]
    }
    type |-[I, O] = Impl[I, O]

    object asTupleMapper extends TupleX.Mapper {

      implicit def rewrite[I, R](
          implicit
          _case: I |- R // TODO: should be a lemma, but spoiled by Scala's widen to Any problem
      ): asTupleMapper.this.Case.Aux[I, R] = at[I] { v =>
        _case.apply(v)
      }
    }

    type BuildTarget[I, O] = Impl[I, O]

    protected def build[I, O](fn: I => O)(
        implicit
        _definedAt: SrcDefinition
    ): BuildTarget[I, O] = {

      val _case = Tag(Fn.at[I](fn)) <> CaseTag
      _case
    }

    // TODO: all these cases can only be summoned when Poly is path-dependent, is there an API that works otherwise?

    def apply[I](v: I)(
        implicit
        _case: Lemma.At[I]
    ): _case.Out = {
      _case.apply(v)
    }

    case class DomainBuilder[I, O]() extends IDomainBuilder[I, O] {

      type _Lemma = Lemma[I, O]
      type _Impl = Impl[I, O]
      type _Native = (I => O)

      def summon(
          implicit
          _case: Lemma[I, O]
      ): _case.type = _case
    }
    def domainBuilder[i, o]: DomainBuilder[i, o] = DomainBuilder()
  }

  object Poly {}

  implicit class PolyOps[P <: Poly](self: P) extends Serializable {}
}
