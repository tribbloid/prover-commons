package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.util.NamedArgs
import shapeless.SingletonProductArgs

trait HasMorphism extends Tier {

  // dedicated to polymorphic functions
  // Morphism takes type argument, Poly takes an implicit type class
  // relying heavily on kind-projector plugin: https://github.com/typelevel/kind-projector
  // please refrain from using shortened syntax as it is very different from that of Scala3
  // TODO: Unfortunately, from this point, shapeless Poly & DepFn are almost useless
  //  just like Scala2 Function is almost useless
  //  DepFn is not really a function with dependent type
  //  Poly is unbounded, doesn't have impl for Morphism, has poor compatibility with Scala3,
  //  also, each case doesn't have an output dependent type (like DepFn)

  trait Bound {

    type /\
    type \/ <: /\
    // TODO: can this be simplified?

    trait Morphic[
        -I[_ >: \/ <: /\] <: HUB,
        +R[_ >: \/ <: /\]
    ] extends PolyLike {

      def specific[T >: \/ <: /\]: FnCompat[I[T], R[T]]

      final def argsApply[T >: \/ <: /\](args: NamedArgs[I[T]]): R[T] = specific[T].argsGet(args)
    }
  }

  object NoBound extends Bound {
    type /\ = Any
    type \/ = Nothing
  }

  trait MorphismDynamics[
      -I[_] <: HUB,
      +R[_]
  ] extends SingletonProductArgs {
    self: Morphism[I, R] =>

    final def applyProduct[T](args: I[T]): R[T] = {

      self.argsApply(NamedArgs(args))
    }
  }

  /**
    * a.k.a. parametric polymorphism, e.g. natural transformation
    *
    * can take a type argument and generate a specific [[FnCompat]]
    *
    * obviously, [[FnCompat]] itself is a trivial case of morphic that always generates itself (which explained the
    * implicit cast from it)
    *
    * serve as the basis of functions with dependent type
    *
    * @tparam I
    *   type constructor(s) of input arg(s)
    * @tparam R
    *   type constructor of output
    */
  trait Morphism[
      -I[_] <: HUB,
      +R[_]
  ] extends NoBound.Morphic[I, R]
      with MorphismDynamics[I, R] {}

  /**
    * function with dependent type
    *
    * equivalent to `def(v: A): v.R` or `def[T <: A](v: T): T#R` in terms of capability
    * @tparam I
    *   type(s) of input arg(s)
    * @tparam R
    *   type constructor of output
    */
  trait Dependent[
      -I <: HUB,
      +R[_]
  ] extends Morphism[Lambda[t => I], R]
}
