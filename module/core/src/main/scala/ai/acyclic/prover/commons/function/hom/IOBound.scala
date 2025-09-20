package ai.acyclic.prover.commons.function.hom

import scala.language.implicitConversions

trait IOBound {
  // Used to annotate FnImpl when Input is not contravariant & Output is not covariant

  type IMax
  type OMin
}

object IOBound {

  type Compat[-I, +O] = IOBound { type In >: I; type Out <: O }

  case class _View[T <: IOBound](
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

  implicit def view[T <: IOBound](self: T): _View[T] = _View(self)
}
