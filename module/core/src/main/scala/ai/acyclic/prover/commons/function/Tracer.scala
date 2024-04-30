package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.api.FnLike

trait Tracer[+T] {
  def unbox: T
}

object Tracer {

//  case class Applied[I, O](fn: FnLike[],on: I) extends Tracer[I] {
//    def unbox: I = on
//  }
//
//  case class Free[T]() extends Tracer[T] {
//    def unbox: T = throw new RuntimeException("Free variable")
//  }
}
