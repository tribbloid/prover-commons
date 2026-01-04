package ai.acyclic.prover.commons.multiverse.rewrite

import scala.language.implicitConversions

/**
  * CanNormalise, but implicitly, becomes a built-in type in Scala 3
  *
  * useless most of the time, defining an implicit function is much easier
  */
trait Conversion[-T, +R] extends CanNormalise[T, R] with (T => R) {

  implicit override def normalise(v: T): R
}

object Conversion {}
