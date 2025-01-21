package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.Delegating.Unboxing.Conversion

import scala.language.implicitConversions

trait Delegating[+T] {

  import Delegating.*

  def unbox: T

  def as[R](
      implicit
      lemma: Unboxing.Conversion[Delegating[T], R]
  ): R = lemma(this)
}

object Delegating {

  trait Unboxing_Imp0 {

    implicit def unboxMore[T, R](
        implicit
        continuation: Conversion[T, R]
    ): Conversion[Delegating[T], R] = { v =>
      continuation(v.unbox)
    }
  }

  object Unboxing extends Unboxing_Imp0 {
    // not using Poly to avoid overhead

    trait Conversion[-T, +R] extends (T => R) {} // TODO: has a canonical impl in Scala 3

    implicit def unbox1[T]: Conversion[Delegating[T], T] = { v =>
      v.unbox
    }

  }

  implicit def unboxImplicitly[T](v: Delegating[T]): T = v.as

  
}
