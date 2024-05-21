package ai.acyclic.prover.commons.function.hom

import scala.language.implicitConversions

trait Tracing extends Explainable {

  type In
  type Out

  def apply(arg: In): Out
}

object Tracing {

  type Compat[-I, +O] = Tracing { type In >: I; type Out <: O }

  case class _View[T <: Tracing](
      self: T
  ) {

//    def widen[II, OO](
//        implicit
//        ev1: II <:< self.In,
//        ev2: self.Out <:< OO
//    ): T { type In = II; type Out = OO } = {
//
////      self // scala is dumb, can't figure out automatically
//      self.asInstanceOf[T { type In = II; type Out = OO }]
//    }
  }

  implicit def view[T <: Tracing](self: T): _View[T] = _View(self)
}
