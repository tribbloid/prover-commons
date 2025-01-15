package ai.acyclic.prover.commons.function.hom

object Hom extends Hom_Imp0 {

  type :=>[-I, +R] = Fn[I, R]
  val :=> : Fn.type = Fn

  type Poly1[-I[_], +R[_]] = BoundView.top.Poly1[I, R]

  type :|~>[-I[_], +R[_]] = BoundView.top.Poly1[I, R]
  val :|~> = BoundView.top.Poly1

  type :|->[+R[_]] = BoundView.top.Dependent[R]
  val :|-> = BoundView.top.Dependent

  object Impl {

    type Circuit[I, R] = Fn.Impl[I, R]

    type Poly = Hom.Poly

    type Poly1[I[_], R[_]] = BoundView.top.Poly1.Impl[I, R]
  }

//  override type BuildTarget[I, O] = Fn.Impl[I, O]
//
//  case class DomainBuilder[I, O]() extends IDomainBuilder[I, O] {
//    override def makeExact[_I >: I, _R <: O](fn: _I => O)(
//        implicit
//        _definedAt: SrcDefinition
//    ): Fn.Impl[_I, _O] = ???
//  }
//
//  override def copy[I2, O2]: DomainBuilder[I2, O2] = DomainBuilder[I2, O2]()
//
//  implicit def asRoot(v: Hom.type): DomainBuilder[Nothing, Any] = v.rootBuilder
}
