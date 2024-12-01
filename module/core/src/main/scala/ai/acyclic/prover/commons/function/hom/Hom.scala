package ai.acyclic.prover.commons.function.hom

object Hom extends Hom_Imp0 {

  type :=>[-I, +R] = Circuit[I, R]
  val :=> : Circuit.type = Circuit

  type Poly1[-I[_], +R[_]] = TypeDomain.top.Poly1[I, R]

  type :|~>[-I[_], +R[_]] = TypeDomain.top.Poly1[I, R]
  val :|~> = TypeDomain.top.Poly1

  type Dependent[+R[_]] = TypeDomain.top.Dependent[R]

  type :|->[+R[_]] = TypeDomain.top.Dependent[R]
  val :|-> = TypeDomain.top.Dependent

  object Impl {

    type Circuit[I, R] = Circuit.Impl[I, R]

    type Poly = Hom.Poly

    type Poly1[I[_], R[_]] = TypeDomain.top.Poly1.Impl[I, R]

    type Dependent[R[_]] = TypeDomain.top.Dependent.Impl[R]
  }
}
