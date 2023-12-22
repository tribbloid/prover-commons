package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.function.FnSystem
import shapeless.{HList, SingletonProductArgs}

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

      self.argApply(Args(args))
    }
  }

  trait Fn[I <: HUB, R] extends Adjoint.Fn[Args[I], R] with FnDynamics[I, R] {
    // always has implicit conversion to a Scala function
  }

  trait MorphismDynamics[
      -I[_] <: HUB,
      +R[_]
  ] extends SingletonProductArgs {
    self: Morphism[I, R] =>

    final def applyProduct[T](args: I[T]): R[T] = {

      self.argApply(Args(args))
    }
  }

  trait Morphism[
      -I[_] <: HUB,
      +R[_]
  ] extends Adjoint.Morphism[Lambda[t => Args[I[t]]], R]
      with MorphismDynamics[I, R] {}

  trait Dependent[
      -I <: HUB,
      +R[_]
  ] extends Adjoint.Dependent[Args[I], R]

  trait PolyDynamics extends SingletonProductArgs {
    self: Poly =>

    final def applyProduct[I <: HUB, R](args: I)(
        implicit
        _case: Case[Adjoint.FnCompat[Args[I], R]]
    ): R = {

      self.argApply(Args(args))
    }
  }

  trait Poly extends Adjoint.Poly with PolyDynamics
}

object NamedTier {}
