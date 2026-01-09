package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom

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
  ): Tracer[I2, O] = {
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

  // CAUTION: do not add Expr2[T] unless absolutely necessary
  // all reduction rules should be defined for curried form that yields higher order function(s)
}

trait TracerImplicits extends TracerCanChain {

  implicit def _getValue[T](v: Tracer[?, T])(
      implicit
      position: SrcDefinition = null
  ): T =
    v.getValue

  implicit class UnaryView[I, O](private val self: Tracer[I, O]) {

    // minimal requirement for for-comprehension
    def map[OO](right: Var[O] => OO)(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Tracer[I, canChain.Repr] = {
      ???
    }

    def foreach(right: Var[O] => Unit)(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[I, Unit] = {
      ???
    }

    def flatMap[I2, OO](right: Var[O] => Tracer[I2, OO])(
        implicit
        canChain: CanChain[OO],
        _definedAt: SrcDefinition
    ): Tracer[(I, I2), canChain.Repr] = {
      ???
    }

    def withFilter(right: Var[O] => Boolean)(
        implicit
        _definedAt: SrcDefinition
    ): Tracer[I, O] = {

      ???
    }
  }
}
