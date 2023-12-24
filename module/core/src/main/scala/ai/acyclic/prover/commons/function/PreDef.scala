package ai.acyclic.prover.commons.function


object PreDef extends FnSystem {

  type IUB = Any

  type :=>[I, R] = Fn[I, R]

  implicit class FnOps[I, R](val self: FnCompat[I, R]) {

    def apply(args: I): R = self.apply(args)

    def andThen[R2](g: FnCompat[R, R2]): AndThen[I, R, R2] = {
      AndThen(self, g)
    }
  }

  case class AndThen[I, R, R2](
      f: FnCompat[I, R],
      g: FnCompat[R, R2]
  ) extends DerivedFn[I, R2](
        { ii =>
          val r: R = f.apply(ii)
          val r2: R2 = g.apply(r.asInstanceOf[R with IUB])
          r2: R2
        }
      )(f)

  type :|~>[-I[_] <: IUB, +R[_]] = Morphism[I, R]
  type :|=>[-I, +R[_]] = Dependent[I, R]

  implicit class MorphismOps[-I[_] <: IUB, +R[_]](self: Morphism[I, R]) {

    def apply[T](args: I[T]): R[T] = self.apply(args)
  }

  implicit class PolyOps(self: Poly) {

    def apply[I, R](arg: I)(
        implicit
        _case: self.Case[FnCompat[I, R]]
    ): R = self.apply(arg)(_case)
  }

}
