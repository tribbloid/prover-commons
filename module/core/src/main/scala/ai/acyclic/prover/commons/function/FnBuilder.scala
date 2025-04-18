package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.debug.SrcDefinition

object FnBuilder {

  trait Root extends FnBuilder[Nothing, Any] {

    protected type BuildTarget[I, O]

    protected def build[I, O](fn: I => O)(
        implicit
        _definedAt: SrcDefinition
    ): BuildTarget[I, O]

    trait IDomainBuilder[I, O] extends FnBuilder[I, O] {
      override type DomainBuilder[i, o] = Root.this.DomainBuilder[i, o]

      override def domainBuilder[i, o]: DomainBuilder[i, o] = Root.this.domainBuilder[i, o]

      def apply[o <: O](fn: I => o)(
          implicit
          _definedAt: SrcDefinition
      ): BuildTarget[I, o] = {
        Root.this.build(fn)
      }
    }
  }
}

trait FnBuilder[I, O] {

  type DomainBuilder[_, _]

  def domainBuilder[i, o]: DomainBuilder[i, o] // TODO: need to be tighter
  final def at[i]: DomainBuilder[i, O] = domainBuilder[i, O]
  final def to[o]: DomainBuilder[I, o] = domainBuilder[I, o]
}
