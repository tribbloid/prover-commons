package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom
import ai.acyclic.prover.commons.function.tracing.Expr.Const

import scala.language.implicitConversions

trait Tracer[-I, +O] {

  def getValue(
      implicit
      position: SrcDefinition
  ): O

  // beta reduction,
  def apply[I2](arg: Tracer[I2, I])(
      implicit
      _definedAt: SrcDefinition
  ): Expr._1[I2, O] = {
    ???
  }

  // enable currying, calculus of variations
  def liftToHigherOrder(
      implicit
      _definedAt: SrcDefinition
  ): Tracer[Unit, Hom.Fn[I, O]] = {
    ???
  }

  // stolen form ZIO ZLayers, these are shorthands for defining parallel computation graphs, they are not necessary but can make definition shorter

  trait zipLike {

    def apply[I2, O2](right: Tracer[I2, O2])(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[(I, I2), (O, O2)]
  }

  object zip extends zipLike {
    override def apply[I2, O2](right: Tracer[I2, O2])(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[(I, I2), (O, O2)] = ???
  }
  def <*> = zip

  //  object zipPar extends zipLike {
  //    override def apply[I2, O2](right: TracingV2[I2, O2])(
  //      implicit
  //      _definedAt: SrcDefinition
  //    ): TracingV2[(I, I2), (O, O2)] = ???
  //  }
  //  def <&> = zipPar

  object union {

    def apply[I2 <: I, O2](right: Tracer[I2, O2])(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[I2, (O, O2)] = {
      ???
    }
  }

  object OrElse {}
  def <> = OrElse

  object OrElseEither {}
  def <+> = OrElseEither
}

object Tracer extends TracerCanChain with TracerImplicits {

  type Endo[T] = Tracer[T, T]

  type Gen[+T] = Tracer[Any, T]

  implicit def unzipVar[I, A, B](
      v: Tracer[I, (A, B)]
  )(
      implicit
      pos: SrcDefinition
  ): (Tracer[I, A], Tracer[I, B]) = {
    val v1: Tracer[I, A] = new UnaryView(v).map(v => v.getValue._1)
    val v2: Tracer[I, B] = new UnaryView(v).map(v => v.getValue._2)
    (v1, v2)
  }

  implicit def _asConst[T](v: T): Const[T] = Const(v)

  // UnaryView moved to TracerImplicits

  // CAUTION: do not add Expr2[T] unless absolutely necessary
  // all reduction rules should be defined for curried form that yields higher order function(s)
}
