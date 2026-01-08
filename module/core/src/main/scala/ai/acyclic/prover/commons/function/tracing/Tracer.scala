package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom

trait Tracer[I, O] {

  def getValue(
      implicit
      position: SrcDefinition
  ): O

  // minimal requirement for for-comprehension
  def map[O2](right: Var[O] => O2)(
      implicit
      _definedAt: SrcDefinition
  ): Tracer[I, O2] = {
    ???
  }

  def foreach(right: Var[O] => Unit)(
      implicit
      _definedAt: SrcDefinition
  ): Tracer[I, Unit] = {
    ???
  }

  def flatMap[I2, O2](right: Var[O] => Tracer[I2, O2])(
      implicit
      _definedAt: SrcDefinition
  ): Tracer[(I, I2), O2] = {
    ???
  }

  def withFilter(right: Var[O] => Boolean)(
      implicit
      _definedAt: SrcDefinition
  ): Tracer[I, O] = {

    ???
  }

  // beta reduction,
  def apply(arg: Var[I])(
      implicit
      _definedAt: SrcDefinition
  ): Concrete[O] = {
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

    def apply[I2 >: I, O2](right: Tracer[I2, O2])(
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

object Tracer {

  implicit def _get[T](v: Tracer[?, T])(
      implicit
      position: SrcDefinition
  ): T =
    v.getValue

  // CAUTION: do not add Expr2[T] unless absolutely necessary
  // all reduction rules should be defined for curried form that yields higher order function(s)
}
