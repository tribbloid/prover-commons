package ai.acyclic.prover.commons.function

trait HasPolyLike extends HasFn {

  trait PolyLike extends FnLike {

    trait IsCase extends FnLike.Cap
    // TODO: should be a type

    type Case[+FF <: FnBase[_]] = FF with FnLike.Can[IsCase]

    type =>>[I <: IUB, O] = Case[Fn[I, O]]
    type CaseFrom[I <: IUB] = Case[FnBase[I]]

    class CaseBuilder[I <: IUB, F <: FnBase[I]] {

      def to[O]: CaseBuilder[I, FnCompat[I, O]] = new CaseBuilder[I, FnCompat[I, O]]
      def =>>[O]: CaseBuilder[I, FnCompat[I, O]] = to[O]

      def defining[FF <: F](fn: FF): Case[FF] = fn.enable[IsCase]
      def apply[FF <: F](fn: FF): Case[FF] = defining(fn)

      def defining[R](fn: I => R)(
          implicit
          ev: Fn[I, R] <:< F
      ): Case[Fn[I, R]] = Fn(fn).enable[IsCase]
      def apply[R](fn: I => R)(
          implicit
          ev: Fn[I, R] <:< F
      ): Case[Fn[I, R]] = defining(fn)

      def summon(
          implicit
          _case: Case[F]
      ): _case.type = _case
    }

    // similar to `at` in shapeless Poly1
    def at[I <: IUB] = new CaseBuilder[I, FnBase[I]]

    def getCaseFor[I <: IUB](v: I)(
        implicit
        _case: Case[FnBase[I]]
    ): _case.type = _case
  }
}
