package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.util.SrcExplainable

import scala.language.implicitConversions

trait Traced extends SrcExplainable {

  type In
  type Out
}

object Traced {

  type Compat[-I, +O] = Traced { type In >: I; type Out <: O }

  case class _View[T <: Traced](
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

  implicit def view[T <: Traced](self: T): _View[T] = _View(self)
}
