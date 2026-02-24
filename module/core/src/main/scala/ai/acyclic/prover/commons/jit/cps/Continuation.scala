package ai.acyclic.prover.commons.jit.cps

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.multiverse.rewrite.Delegating
import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}

import scala.language.implicitConversions

/**
  * here are the rules:
  *
  *   - all methods under it becomes extension methods in [[ai.acyclic.prover.commons.jit.tracing.FnImp0]] or its
  *     supertypes
  *   - when creating new instance of [[Continuation]], use existing constructors if possible, ask first before adding
  *     new constructor
  *   - all compilation and tests should succeed
  *   - do not remove or comment out code, specifically code in
  *     [[ai.acyclic.prover.commons.jit.tracing.__TracingDesign]]
  *   - the old class should be removed at last
  *   - your code outside the main package should be minimal
  */
case class Continuation[I <: Args.Prod, +O](
    self: Hom.Fn[I, O]
) extends Delegating[Hom.Fn[I, O]] {

  lazy val higherOrder: Continuation[T0, Hom.Fn[I, O]] = {
    Continuation(Hom.Fn.provided0(self))
  }

  def map[O2](right: O => O2)(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, O2] = {

    val _right: Hom.Fn[O ><: T0, O2] = Hom.Fn.at[O](
      right
    )(_definedAt)

    val result =
      Hom.Fn
        .Mapped[I, O, O2](self, _right)
        .simplify // Mapped[I, O, O2].simplify returns Fn[I, O2]

    Continuation(result)
  }

  def flatMap[O2](right: O => Continuation[I, O2])(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, O2] = {

    map(right).flatten
  }

  def foreach(right: O => Unit)(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, Unit] = {

    map(right)
  }

  def withFilter(right: O => Boolean)(
      implicit
      _definedAt: SrcDefinition
  ): Continuation[I, O] = {

    val _right = Hom.Fn.Blackbox[O, O](_definedAt) { v =>
      if (right(v)) v // v is already O (Blackbox[O, O] receives O)
      else throw new MatchError(s"condition ${_definedAt} is not applicable on $v")
    }

    val result =
      Hom.Fn.Mapped[I, O, O](self, _right)

    Continuation(result.simplify) // Mapped[I, O, O].simplify returns Fn[I, O]
  }

  object zip {

    def apply[I2 <: Args.Prod, O2, Z <: Args.Prod](
        right: Continuation[I2, O2]
    )(
        implicit
        unzip: Args.Zippable.Aux[I, I2, Z]
    ): Continuation[Z, (O, O2)] = {

      Continuation(Hom.Fn.zip(self, right.self))
    }
  }
  def <*> = zip

  object fork {

    def apply[I2 <: Args.Prod, O2, Z <: Args.Prod](
        right: Continuation[I2, O2]
    )(
        implicit
        unzip: Args.Zippable.Aux[I, I2, Z]
    ): Continuation[Z, (O, O2)] = {

      Continuation(Hom.Fn.zip(self, right.self))
    }
  }
  def <%> = fork

  // flatMap is undefined, there are several options, see dottyspike ForComprehension spike for details

  override lazy val unbox: Hom.Fn[I, O] = self.simplify
}

object Continuation {
  // Implicit conversions are provided by Delegating.unbox1

  // Additional implicit conversion from Tracing to Function1View for function composition
  implicit def tracingToFunction[I <: Args.Prod, O](v: Continuation[I, O])(
      implicit
      _definedAt: SrcDefinition
  ): Function1[I, O] = {
    new Function1[I, O] {
      override def apply(arg: I): O = v.unbox(arg)
    }
  }

  implicit class CanFlatten[I <: Args.Prod, O](continuation: Continuation[I, Continuation[I, O]]) {

    def flatten(
        implicit
        _definedAt: SrcDefinition // TODO: this should not be required
    ): Continuation[I, O] = {

      val result =
        Hom.Fn.Flatten[I, Continuation[I, O], O](
          continuation.self,
          { v: Continuation[I, O] => v.self }
        )

      Continuation(result.simplify) // Flatten[I,_,O].simplify returns Fn[I, O]
    }
  }
}
