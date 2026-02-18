package ai.acyclic.prover.commons.jit.hom

import ai.acyclic.prover.commons.tag.Tag
import ai.acyclic.prover.commons.tag.<>
import ai.acyclic.prover.commons.compat.TupleX
import ai.acyclic.prover.commons.jit.{ComputationGraph, FnBuilder}
import ai.acyclic.prover.commons.debug.SrcDefinition

trait HasPoly extends HasFunction {

  trait PolyLike extends Serializable with ComputationGraph {}

  /**
    * Ad-hoc polymorphic function, the most flexible polymorphism
    *
    * contains several cases, each take a type argument and generate a specific [[Case]]
    *
    * the exact case being selected for function application should be determined in compile-time (by the implicit
    * evidence), doing it in runtime is shunned in type theories (it is fine in set theories tho), but we may still
    * allow it (if not obstructed by type erasure)
    *
    * due to the lack of path-dependent implicit search, all instances of Poly have to be singleton objects to make
    * their cases visible
    *
    * test cases for
    */
  abstract class Poly(
      implicit
      override val _definedAt: SrcDefinition
  ) extends PolyLike
      with FnBuilder.Root {
    // TODO: construction/inference of Case should be interned

    protected[Poly] case class Case[I, O]( // a thin wrapper that prevents apply from being called directly
        underlying: Fn.Impl[I, O]
    ) extends Fn.Impl[I, O] {
      def apply(v: I): O = underlying(v)
    }
    protected[Poly] object Case {
      type At[I] = Case[I, ?]
    }

    /**
      * Horizontal line in Gentzen's notation for posterior in deduction rule
      */
    protected[Poly] type /=>[I, O] = Case[I, O]

    type Lemma[-I, +O] = Case[? >: I, ? <: O]
    object Lemma {
      type At[I] = Lemma[I, ?]
    }

    /**
      * Turnstile in Gentzen's notation for priors in deduction rule ([[Lemma]])
      */
    type |-[-I, +O] = Lemma[I, O]

    object asTupleMapper extends TupleX.Mapper {

      implicit def rewrite[I, R](
          implicit
          _case: I /=> R // TODO: should be a lemma, but spoiled by Scala's widen to Any problem
      ): asTupleMapper.this.Case.Aux[I, R] = at[I] { v =>
        _case.apply(v)
      }
    }

    type BuildTarget[I, O] = Case[I, O]

    protected def build[I, O](fn: I => O)(
        implicit
        _definedAt: SrcDefinition
    ): BuildTarget[I, O] = {

      Case(Fn.at[I](fn))
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
      type _Impl = Case[I, O]
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
