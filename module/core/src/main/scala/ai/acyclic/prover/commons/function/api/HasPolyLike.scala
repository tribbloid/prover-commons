package ai.acyclic.prover.commons.function.api

trait HasPolyLike extends HasFn {

  trait PolyLike extends FnLike {

    trait IsCase extends FnLike.Cap
    // TODO: should be a type

    type Case[+FF <: Fn[_]] = FF with FnLike.Can[IsCase]

    type =>>[I <: IUB, O] = Case[FnImpl[I, O]]
    type CaseFrom[I <: IUB] = Case[Fn[I]]

    class CaseBuilder[I <: IUB, F <: Fn[I]] {

      def to[O]: CaseBuilder[I, FnCompat[I, O]] = new CaseBuilder[I, FnCompat[I, O]]
      def =>>[O]: CaseBuilder[I, FnCompat[I, O]] = to[O]

      def defining[FF <: F](fn: FF): Case[FF] = fn.enable[IsCase]
      def apply[FF <: F](fn: FF): Case[FF] = defining(fn)

      def defining[R](fn: I => R)(
          implicit
          ev: FnImpl[I, R] <:< F
      ): Case[FnImpl[I, R]] = Fn(fn).enable[IsCase]
      def apply[R](fn: I => R)(
          implicit
          ev: FnImpl[I, R] <:< F
      ): Case[FnImpl[I, R]] = defining(fn)

      def summon(
          implicit
          _case: Case[F]
      ): _case.type = _case
    }

    // similar to `at` in shapeless Poly1
    def at[I <: IUB] = new CaseBuilder[I, Fn[I]]

    def getCaseFor[I <: IUB](v: I)(
        implicit
        _case: Case[Fn[I]]
    ): _case.type = _case
  }
}
