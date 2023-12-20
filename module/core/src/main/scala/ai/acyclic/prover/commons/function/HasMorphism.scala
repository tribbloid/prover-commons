package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.util.NamedArgs
import shapeless.SingletonProductArgs

trait HasMorphism extends Tier {

  // section dedicated to polymorphic functions
  // obviously, T0 doesn't have it
  // relying heavily on kind-projector plugin: https://github.com/typelevel/kind-projector
  // please refrain from using shortened syntax as it is very different from that of Scala3
  // TODO: Unfortunately, from this point, shapeless Poly & DepFn are almost useless
  //  just like Scala2 Function is useless
  //  Poly is unbounded, and DepFn is not really a function with dependent type

  trait Bound {

    type /\
    type \/ <: /\
    // TODO: can this be simplified?

    trait Morphic[
        -H[_ >: \/ <: /\] <: HUB,
        +R[_ >: \/ <: /\]
    ] extends PolyLike {

      def specific[T >: \/ <: /\]: Function[H[T], R[T]]

      final def argsApply[T >: \/ <: /\](args: NamedArgs[H[T]]): R[T] = specific[T].argsGet(args)
    }
  }

  object NoBound extends Bound {
    type /\ = Any
    type \/ = Nothing
  }

  trait MorphismDynamics[
      -H[_] <: HUB,
      +R[_]
  ] extends SingletonProductArgs {
    self: Morphism[H, R] =>

    final def applyProduct[T](args: H[T]): R[T] = {

      self.argsApply(NamedArgs(args))
    }
  }

  /**
    * a.k.a. parametric polymorphism, e.g. natural transformation
    *
    * can take a type argument and generate a specific [[Function]]
    *
    * obviously, [[Function]] itself is a trivial case of morphic that always generates itself (which explained the
    * implicit cast from it)
    *
    * serve as the basis of functions with dependent type
    *
    * @tparam H
    *   type constructor(s) of input arg(s)
    * @tparam R
    *   type constructor of output
    */
  trait Morphism[
      -H[_] <: HUB,
      +R[_]
  ] extends NoBound.Morphic[H, R]
      with MorphismDynamics[H, R] {}

  /**
    * function with dependent type
    *
    * equivalent to `def(v: A): v.R` or `def[T <: A](v: T): T#R` in terms of capability
    * @tparam H
    *   type(s) of input arg(s)
    * @tparam R
    *   type constructor of output
    */
  trait Dependent[
      -H <: HUB,
      +R[_]
  ] extends Morphism[Lambda[t => H], R]
}
