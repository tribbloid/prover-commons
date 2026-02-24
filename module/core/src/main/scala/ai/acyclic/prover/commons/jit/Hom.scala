package ai.acyclic.prover.commons.jit

import ai.acyclic.prover.commons.TypeTag
import ai.acyclic.prover.commons.jit.eval.Args
import Args.T0

object Hom extends hom.Hom_Imp0 {

  type :=>[-I <: Args, +R] = Fn[I, R]
  val :=> : Fn.type = Fn

  type :|~>[-I[_], +R[_]] = BoundView.top.UnnaturalTransformation[I, R]
  val :|~> = BoundView.top.UnnaturalTransformation

  //  type :|->[+R[_]] = BoundView.top.Dependent[R] // TODO: superseded
  //  val :|-> = BoundView.top.Dependent

  type Dependent[+R[_]] = DepFn[Args] { type OutK[T] <: R[T] }
  type :|->[+R[_]] = Dependent[R]

  object Impl {

    type Fn[I <: Args, R] = Fn.Impl[I, R]

    //    type Poly = Hom.Poly

    type UnnaturalTransformation[I[_], R[_]] = BoundView.top.UnnaturalTransformation.Impl[I, R]
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
