package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.function.api.{Explainable, SystemBase}

import scala.language.implicitConversions

trait HomBase extends SystemBase {
  self: Singleton =>

  final type IUB = Any

  override type _Unit = Unit
  override val _unit: Unit = ()

  implicit class FnRepr[I, O](val self: FnCompat[I, O]) extends Serializable with (I => O) {

    override def apply(v1: I): O = self.apply(v1)

    case class Continuation private () {

      def map[O2](next: O => O2): FnImpl[I, O2] = {

        /**
          * [[TracingView]] turning upside-down
          */
        val nextFn: FnImpl[O, O2] = Fn(next)

        val result: FnImpl[I, O2] =
          FnRepr.Compose(self: FnCompat[I, O], nextFn).reduce

        result
      }

      def foreach(fn: O => Unit): FnImpl[I, Unit] = map(fn)

      def flatMap[O2](fn: O => TracerCompat[O2]): FnImpl[I, O2] = {
        ???
      }

//      def apply[O2](fn: O => O2): FnImpl[I, O2] = map(fn) // merely an alias
    }

    lazy val out = Continuation()

    override def andThen[O2](g: O => O2) = {

      val cc: FnImpl[I, O2] = out.map(g)
      val result: FnRepr[I, O2] = FnRepr(cc)
      result
    }
  }

  object FnRepr {

    case class Compose[
        I <: IUB,
        O1,
        O2
    ](
        f: FnCompat[I, O1],
        g: FnCompat[O1, O2]
    ) {

      lazy val reduce: FnImpl[I, O2] = {

        (f, g) match {

          case (_f: Fn.Identity[_], _g) =>
            _g.asInstanceOf[FnImpl[I, O2]]
          case (_f, _g: Fn.Identity[_]) =>
            _f.asInstanceOf[FnImpl[I, O2]]
          case _ =>
            AndThen
        }
      }

      object AndThen extends FnImpl[I, O2] with Explainable.Composite with Explainable.DecodedName {

        override def apply(arg: I): O2 = {
          val o1: O1 = f.apply(arg)
          g.apply(o1)
        }

        override def composedFrom: Seq[Explainable] = Seq(f, g)
      }
    }
  }

  implicit def doubleElimination2[I, O](v: TracerCompat[FnCompat[I, O]]): FnRepr[I, O] = FnRepr(v.unbox)
}
