package ai.acyclic.prover.commons.multiverse.rewrite

import scala.language.implicitConversions

trait HasConversion {

  /**
    * CanNormalise, but implicitly, becomes a built-in type in Scala 3
    *
    * useless most of the time, defining an implicit function is much easier
    */
  trait Conversion[-T, +R] extends CanNormalise[T, R] with (T => R) {}

  infix type %=>[-T, +R] = Conversion[T, R]

  object Conversion {}

  implicit def convert[T, R](v: T)(
      implicit
      conv: Conversion[T, R]
  ): R = conv(v)
}
