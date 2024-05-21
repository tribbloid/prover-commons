package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.function.hom.{Explainable, SystemBase}

import scala.language.implicitConversions

trait System_Imp0 extends SystemBase {
  self: Singleton =>

  implicit class TracerCompatOps[R](val self: TracerCompat[R]) extends Serializable {

    def map[O2](fn: R => O2): TracerImpl[O2] = {

      Fn[R, O2](fn).^.trace_apply(self)
    }

    def foreach(fn: R => Unit): Unit = {
      map(fn).unbox
    }

    def flatMap[O2](fn: R => O2): O2 = {
      ???
    }
  }

  case class MaybeCompose[
      I,
      O1,
      O2
  ](
      f: FnCompat[I, O1],
      g: FnCompat[O1, O2]
  ) {

    lazy val reduce: FnImpl[I, O2] = {

      (f, g) match {

        case (_: Fn.Identity[_], _g) =>
          _g.asInstanceOf[FnImpl[I, O2]]
        case (_f, _: Fn.Identity[_]) =>
          _f.asInstanceOf[FnImpl[I, O2]]
        case _ =>
          Compose
      }
    }

    object Compose extends FnImpl[I, O2] with Explainable.Composite with Explainable.DecodedName {

      override def apply(arg: I): O2 = {
        val o1: O1 = f.apply(arg)
        g.apply(o1)
      }

      override def composedFrom: Seq[Explainable] = Seq(f, g)
    }
  }

  implicit class FnRepr[I, O](val self: FnCompat[I, O]) extends FnReprLike[I, O] with Serializable {

    def _andThen[O2](next: O => O2): FnImpl[I, O2] = {

      val nextFn: FnCompat[O, O2] = Fn[O, O2](next)

      val result: FnImpl[I, O2] =
        MaybeCompose[I, O, O2](self, nextFn).reduce

      result
    }

    def _compose[I1](prev: I1 => I): FnImpl[I1, O] = {

      val prevFn = Fn(prev)

      val result: FnImpl[I1, O] =
        MaybeCompose[I1, I, O](prevFn, self).reduce

      result
    }

    case class Continuation private () {

      def map[O2](next: O => O2): FnImpl[I, O2] = {

        val result = _andThen(next): FnImpl[I, O2]

        result
      }

      def foreach = map[Unit] _

      def flatMap[T](fn: O => T): FnImpl[I, T] = {
        ???
      }
    }

    lazy val out = Continuation()

    override def revert: FnImpl[I, O] = self.widen[I, O]

    override def apply(v1: I): O = self.apply(v1)

    override def andThen[O2](g: O => O2): FnRepr[I, O2] = {

      _andThen(g)
    }

    override def compose[A](g: A => I): FnRepr[A, O] = {
      _compose(g)
    }
  }

  object FnRepr {}

  implicit def tracerToRepr[I, O](v: TracerCompat[FnCompat[I, O]]): FnRepr[I, O] = FnRepr(v.unbox)
}
