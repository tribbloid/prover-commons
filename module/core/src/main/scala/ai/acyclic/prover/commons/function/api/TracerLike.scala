package ai.acyclic.prover.commons.function.api

import scala.language.implicitConversions

trait TracerLike extends Explainable {

  type In
  type Out

  /**
    * the only Single Abstract Method interface
    * @param arg
    *   input (can be product type)
    * @return
    *   output (can be curried function)
    */
  def apply(arg: In): Out

}

object TracerLike {

  case class _View[T <: TracerLike](
      self: T
  ) {

    def widen[II, OO](
        implicit
        ev1: II <:< self.In,
        ev2: self.Out <:< OO
    ): T { type In = II; type Out = OO } = {

//      self // scala is dumb, can't figure out automatically
      self.asInstanceOf[T { type In = II; type Out = OO }]

    }
  }

  implicit def view[T <: TracerLike](self: T): _View[T] = _View(self)
}
