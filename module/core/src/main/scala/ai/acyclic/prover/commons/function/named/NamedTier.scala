package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.function.FnSystem
import shapeless.{HList, SingletonProductArgs}

import scala.language.implicitConversions

abstract class NamedTier {
//  self: Singleton =>
  // CAUTION: DO NOT use Scala 2 tuple form!
  // Scala 3 tuple is equivalent to HList (which itself will be ported to Scala 3 soon)
  // HList also has the extra benefit of being capable of naming its field(s)

  type HUB <: HList

  object Adjoint extends FnSystem {

    final override type IUB = Args[HUB]
  }
  type IUB = Adjoint.IUB

  trait FnDynamics[I <: HUB, R] extends SingletonProductArgs {
    self: Fn[I, R] =>

    final def applyProduct(
        args: I
    ): R = {

      self.self.apply(Args(args))
    }
  }

  case class Fn[I <: HUB, R](self: Adjoint.Fn[Args[I], R]) extends FnDynamics[I, R] {
    // always has implicit conversion to a Scala function
  }

  implicit def vanillaToFn[I <: HUB, R](
      fn: Args[I] => R
  ): Fn[I, R] = {
    Fn(
      Adjoint.vanillaToFn(fn)
    )
  }

  trait MorphismDynamics[
      -I[_] <: HUB,
      +R[_]
  ] extends SingletonProductArgs {
    self: Morphism[I, R] =>

    final def applyProduct[T](args: I[T]): R[T] = {

      self.self.apply(Args(args))
    }
  }

  case class Morphism[
      -I[_] <: HUB,
      +R[_]
  ](self: Adjoint.Morphism[Lambda[t => Args[I[t]]], R])
      extends MorphismDynamics[I, R] {}

  type Dependent[
      -I <: HUB,
      +R[_]
  ] = Morphism[Lambda[t => I], R]

  trait PolyDynamics extends SingletonProductArgs {
    self: Poly =>

    final def applyProduct[I <: HUB, R](args: I)(
        implicit
        _case: self.Case[Adjoint.FnCompat[Args[I], R]]
    ): R = {

      self.self.apply(Args(args))
    }
  }

  case class Poly(self: Adjoint.Poly) extends PolyDynamics
}

object NamedTier {}
