package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.api.{HasMorphism, HasPoly}

import scala.language.implicitConversions

trait HomSystem_Imp0 extends HasMorphism with HasPoly with Serializable {

  final type IUB = Any

  case class FnAsFunction1[I, R](fn: FnCompat[I, R]) extends (I => R) {

    final override def apply(v1: I): R = fn.apply(v1)

    final override def toString: String = fn.toString // preserve reference transparency

  }

  implicit def FnAsFunction1[I, R](fn: FnCompat[I, R]): I => R = {
    FnAsFunction1[I, R](fn)
  }
}
