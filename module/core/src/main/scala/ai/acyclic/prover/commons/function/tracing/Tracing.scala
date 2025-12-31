package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.Delegating
import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom

import scala.language.implicitConversions

case class Tracing[I, O](self: Hom.Fn[I, O]) extends Delegating[Hom.Fn.K2_[I, O]] {

  lazy val higherOrder: Tracing[Unit, Hom.Fn[I, O]] =
    Tracing(Hom.Thunk.CachedEager(self))

  def map[O2](right: O => O2)(
      implicit
      _definedAt: SrcDefinition
  ): Tracing[I, O2] = {

    val _right: Hom.Fn[O, O2] = Hom.Fn.at[O](right)(_definedAt)

    val result =
      Hom.Fn.Mapped[I, O, O2](self, _right)

    Tracing(result)
  }

  def flatMap[O2](right: O => Tracing[I, O2])(
      implicit
      _definedAt: SrcDefinition
  ): Tracing[I, O2] = {

    val rightUnboxed: Hom.Fn[I, Hom.Fn[O, O2]] =
      Hom.Fn.Blackbox[I, Hom.Fn[O, O2]](_definedAt) { i: I =>
        Hom.Fn.Blackbox[O, O2](_definedAt) { o: O =>
          right(o).self(i)
        }
      }

    val result = Hom.Fn.FlatMapped[I, O, O2](self, rightUnboxed)

    Tracing(result)
  }

  def foreach(right: O => Unit)(
      implicit
      _definedAt: SrcDefinition
  ): Tracing[I, Unit] = {

    map(right)
  }

  def withFilter(right: O => Boolean)(
      implicit
      _definedAt: SrcDefinition
  ): Tracing[I, O] = {

    val _right = Hom.Fn.Blackbox[O, Boolean](_definedAt)(right)

    val result =
      Hom.Fn.Filtered[I, O](self, _right)

    Tracing(result)
  }

  def ><[I2, O2](right: Tracing[I2, O2]): Tracing[(I, I2), (O, O2)] = {

    val result = Hom.Fn.Pointwise(self, right.self)

    Tracing(result.normalForm)
  }

  def -<[O2](right: Tracing[I, O2]): Tracing[I, (O, O2)] = {

    val first = Hom.Fn.Duplicate[I]()
    val second = Hom.Fn.Pointwise(self, right.self)

    val result = Hom.Fn.Mapped[I, (I, I), (O, O2)](first, second)

    Tracing(result)
  }

  // flatMap is undefined, there are several options, see dottyspike ForComprehension spike for details

  override def unbox: Hom.Fn[I, O] = self.normalForm
}

object Tracing {

  implicit def asFunction1[I, O](v: Tracing[I, O]): I => O = { i =>
    v.unbox(i)
  }
}
