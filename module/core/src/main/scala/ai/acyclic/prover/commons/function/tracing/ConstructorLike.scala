package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom.:=>

import scala.language.implicitConversions

trait ConstructorLike[
    P, // type of pending input in JIT compilation,
    -I,
    +O
] extends Expr[P, I :=> O] {
  // TODO: should be a view of Expr[?, I :=> O]

  // beta reduction, notice that P is contravariant, and Expr[Any, I] represents a static I,
  // so Constructor[Any, I, O] can apply on any Expr[P, I]
  def apply[PP <: P](arg: Expr[PP, I])(
      implicit
      _definedAt: SrcDefinition
  ): Expr._1[P, O] = {

    ???
  }

  // enable currying, calculus of variations
//  def liftToHigherOrder( // TODO: remove, this should happen automatically
//      implicit
//      _definedAt: SrcDefinition
//  ): Constructor[Unit, I :=> O] = {
//    ???
//  }

  // stolen form ZIO ZLayers, these are shorthands for defining parallel computation graphs, they are not necessary but can make definition shorter

  trait zipLike {

    def apply[I2, O2](right: ConstructorLike[P, I2, O2])(
        implicit
        _definedAt: SrcDefinition
    ): ConstructorLike[P, (I, I2), (O, O2)]
  }

  object zip extends zipLike {
    override def apply[I2, O2](right: ConstructorLike[P, I2, O2])(
        implicit
        _definedAt: SrcDefinition
    ): ConstructorLike[P, (I, I2), (O, O2)] = ???
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

    def apply[I2 <: I, O2](right: ConstructorLike[P, I2, O2])(
        implicit
        _definedAt: SrcDefinition
    ): ConstructorLike[P, I2, (O, O2)] = {
      ???
    }
  }

  object OrElse {}
  def <> = OrElse

  object OrElseEither {}
  def <+> = OrElseEither
}

object ConstructorLike extends ConstructorImplicits {

//  implicit def unzipVar[I, A, B](
//      v: Constructor[I, (A, B)]
//  )(
//      implicit
//      pos: SrcDefinition
//  ): (Constructor[I, A], Constructor[I, B]) = {
//    val v1: Constructor[I, A] = new UnaryView(v).map(v => v.getValue._1)
//    val v2: Constructor[I, B] = new UnaryView(v).map(v => v.getValue._2)
//    (v1, v2)
//  }

  // UnaryView moved to TracerImplicits

  // CAUTION: do not add Expr2[T] unless absolutely necessary
  // all reduction rules should be defined for curried form that yields higher order function(s)
}
