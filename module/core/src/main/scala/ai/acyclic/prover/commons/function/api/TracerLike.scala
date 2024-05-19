package ai.acyclic.prover.commons.function.api

import scala.language.implicitConversions

trait TracerLike extends Explainable {

  type Repr
}

object TracerLike {

  type Lt[+T] = TracerLike { type Repr <: T }

  case class _View[T <: TracerLike](
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
//
//    }
  }

  implicit def view[T <: TracerLike](self: T): _View[T] = _View(self)
}
