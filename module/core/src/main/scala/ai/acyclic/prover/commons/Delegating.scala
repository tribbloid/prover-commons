package ai.acyclic.prover.commons

import scala.language.implicitConversions

/**
  * Mimic the behaviour of delegate feature in Kotlin.
  *
  * allowing methods of [[unbox]] to be called on this object directly
  *
  * if [[unbox]] is also a Delegating, then methods of its [[unbox]] can be called on this object recursively
  *
  * @tparam T
  *   type of the object which this object can delegate to
  */
trait Delegating[+T] {
  // TODO: can be merged into "multiverse.CanNormalise"

  import Delegating.*

  def unbox: T

  def as[R](
      implicit
      lemma: Conversion[Delegating[T], R]
  ): R = lemma(this)
}

object Delegating {

  trait Conversion[-T, +R] extends (T => R) {} // TODO: has a canonical impl in Scala 3

  trait Conversion_Imp0 {

    implicit def unboxMore[T, R](
        implicit
        lemma: Conversion[T, R]
    ): Conversion[Delegating[T], R] = { v =>
      lemma(v.unbox)
    }
  }

  object Conversion extends Conversion_Imp0 {
    // not using Poly to avoid overhead

    implicit def unbox1[T]: Conversion[Delegating[T], T] = { v =>
      v.unbox
    }
  }

  implicit def unboxImplicitly[T, R](v: Delegating[T])(
      implicit
      lemma: Conversion[Delegating[T], R]
  ): R = v.as(lemma)

  // TODO: this only works for 2 levels of nesting WITHOUT dilemma, need at least 5 levels
  //  impl can also be simpler
  implicit def unboxImplicitlyNested[T, R](v: Delegating[Delegating[T]])(
      implicit
      lemma: Conversion[Delegating[T], R]
  ): R = lemma(v.unbox)
}
